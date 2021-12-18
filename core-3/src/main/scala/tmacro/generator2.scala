package tan
package tmacro

import cats.Monad

import tmacro.mirror1.{HttpMethod1, ControllerMirror1, AnnotationMirror1, DefaultBodyMirror1}
import tmacro.mirror2.{ControllerMirror2, MethodInput2, NamingConvention2, PathSegment2}

import scala.quoted.*
import sttp.tapir.*
import sttp.tapir.typelevel.*
import tan.attrs.DefTags

import scala.util.matching.Regex

object generator2 {
  private val camelCaseWordBoundary: Regex = "[A-Z\\d]".r

  class FakeParamConcat[L, R, LR](override val leftArity: Int, override val rightArity: Int) extends ParamConcat[L, R] {
    override type Out = LR
  }

  def rename(name: String, conv: NamingConvention2): String = {
    if (conv == NamingConvention2.AsIs) return name

    val sep = conv match {
      case NamingConvention2.Snake => "_"
      case NamingConvention2.SpaceSnake => " "
      case NamingConvention2.Kebab => "-"
      case _ => "?"
    }

    camelCaseWordBoundary.replaceAllIn(name, sep + _.group(0).toLowerCase())
  }

  def apply[Cls: Type, F[_]: Type, S: Type](using q: Quotes)(clsExpr: Expr[Cls], mirror: ControllerMirror2[q.reflect.Symbol, q.reflect.TypeRepr]): Expr[List[S]] = {
    import q.reflect._
    import q.reflect.report._

    case class InputWithTree[I <: MethodInput2[TypeRepr]](input: I, term: Term)
    case class IndexedInput(input: MethodInput2[TypeRepr], term: Term, index: Int)

    val clsSym = TypeRepr.of[Cls].typeSymbol

    val unitSym = TypeRepr.of[Unit].typeSymbol
    val stringSym = TypeRepr.of[String].typeSymbol

    def summon(repr: TypeRepr): Option[Term] = {
      val tpe = repr.asType.asInstanceOf[Type[Any]]

      Expr.summon(using tpe).map(_.asTerm)
    }

    def typeApply(tcc: TypeRepr, tpes: TypeRepr*): TypeRepr = {
      val tc = tcc match {
        case AppliedType(tc, _) => tc
        case tc => tc
      }

      tc.appliedTo(tpes.toList)
    }

    val tapirInteropRoot: Term = Ref(Symbol.requiredModule("tan.TapirInterop"))
    def tapir(name: String): Term = Select.unique(tapirInteropRoot, name)

    def ttree(repr: TypeRepr): TypeTree = TypeTree.of(using repr.asType)

    val eic = TypeRepr.of[EndpointInputConstructor[_, _]]
    val eoc = TypeRepr.of[EndpointOutputConstructor[_, _]]
    val pei = TypeRepr.of[ProvidedEndpointInput[_]]
    val peo = TypeRepr.of[ProvidedEndpointOutput[_]]

    val endpoints = mirror.methods.map { method =>
      val endpointBase = Apply(
        tapir("makeEndpoint"),
        List(
          method.method match {
            case HttpMethod1.Get => '{ HttpMethod1.Get }.asTerm
            case HttpMethod1.Post => '{ HttpMethod1.Post }.asTerm
            case HttpMethod1.Put => '{ HttpMethod1.Put }.asTerm
            case HttpMethod1.Delete => '{ HttpMethod1.Delete }.asTerm
          },
          Expr.ofList((mirror.tag.toList ++ method.tag).map(t => Expr(t))).asTerm,
          Literal(StringConstant(method.summary.getOrElse(rename(method.name, NamingConvention2.SpaceSnake).capitalize)))
        )
      )

      val pathInputs = method.inputs.collect {
        case p: MethodInput2.Path[TypeRepr] => InputWithTree(p, '{ ??? }.asTerm)
      }

      def defBodyToTag(defBody: DefaultBodyMirror1): TypeRepr =
        defBody match {
          case AnnotationMirror1.Json => TypeRepr.of[DefTags.JsonDefTag]
        }

      val nonPathInputs = method.inputs.collect {
        case np: MethodInput2.Query[TypeRepr] =>
          val tree = Apply(TypeApply(tapir("query"), List(ttree(np.tpe))), List(Literal(StringConstant(np.name))))

          InputWithTree(np, tree)
        case np: MethodInput2.Body[TypeRepr] =>
          val dealiased = np.tpe.dealias
          np.bodyType match {
            case Some(AnnotationMirror1.Json) =>
              summon(typeApply(eic, dealiased, TypeRepr.of[DefTags.JsonDefTag])) match {
                case None => errorAndAbort(s"Failed to find PEI or EIC for type ${np.tpe} (dealiased to $dealiased) (note: this is explicit json body)")
                case Some(eic) => InputWithTree(np, Select.unique(eic, "instance"))
              }
            case Some(AnnotationMirror1.PlainText) =>
              if (dealiased.typeSymbol == stringSym) {
                InputWithTree(np, TypeApply(tapir("plainBody"), List(TypeTree.of[String])))
              } else {
                errorAndAbort(s"Invalid/unsupported type for plainBody input: $dealiased (expecting String)")
              }
            case None =>
              summon(typeApply(pei, dealiased)) match {
                case None => mirror.defaultBody match {
                  case Some(defBody) => summon(typeApply(eic, dealiased, defBodyToTag(defBody))) match {
                    case None => errorAndAbort(s"Failed to find PEI or EIC for type ${np.tpe} (dealiased to $dealiased), also tried to check explicit annots")
                    case Some(t) => InputWithTree(np, Select.unique(t, "instance"))
                  }
                }
                case Some(t) => InputWithTree(np, Select.unique(t, "instance"))
              }
          }
      }

      if (method.inputs.exists(_.isInstanceOf[MethodInput2.Security[TypeRepr]]))
        errorAndAbort("Security inputs are not supported now")

      val indexedInputs = {
        /* if (secInputs.size > 1) {
          errorAndAbort("Multiple secure endpoints are not supported now")
        } */

        val indexedPathInputs = pathInputs.map { p =>
          IndexedInput(
            input = p.input,
            term = '{ ??? }.asTerm,
            index = method.pathSegments.collect { case s: PathSegment2.Subst => s }.indexWhere(_.name == p.input.name)
          )
        }

        val indexedNonPathInputs = nonPathInputs.zipWithIndex.map {
          case (np, i) => IndexedInput(np.input, np.term, i + indexedPathInputs.size)
        }

        /*
        val indexedSecInputs = secInputs.map { np =>
          IndexedInput(np.input, np.tree, -1)
        }*/

        val indexedInputsUnordered = indexedPathInputs /* ++ indexedSecInputs */ ++ indexedNonPathInputs

        method.inputs.map { i => indexedInputsUnordered.find(_.input == i).get }
      }

      def makeParamConcat(l: TypeRepr, r: TypeRepr, leftAr: Int): (TypeRepr, Int, TypeRepr, Term) = {
        val rightAr = if (r.typeSymbol == unitSym) 0 else 1

        val lr = if (rightAr == 0) l else leftAr match {
          case 0 => r
          case 1 => typeApply(TypeRepr.of[Tuple2[_, _]], l, r)
          case _ =>
            val AppliedType(_, ls) = l
            Applied(TypeIdent(defn.TupleClass(leftAr + rightAr)), (ls :+ r).map(ttree)).tpe
        }

        val concat = Apply(TypeApply(Select.unique(New(ttree(typeApply(TypeRepr.of[FakeParamConcat[_, _, _]], l, r, lr))), "<init>"), List(l, r, lr).map(ttree)), List(Literal(IntConstant(leftAr)), Literal(IntConstant(rightAr))))

        (lr, leftAr + rightAr, typeApply(TypeRepr.of[ParamConcat.Aux[_, _, _]], l, r, lr), concat)
      }

      val (withPathInputs, pathInputsLr, arityAfterPath) = method.pathSegments.foldLeft((endpointBase, TypeRepr.of[Unit], 0)) { (ewa, segment) =>
        segment match {
          case PathSegment2.Raw(name) =>
            val (expr, prevLr, leftAr) = ewa
            val (lr, newArity, _, pc) = makeParamConcat(prevLr, TypeRepr.of[Unit], leftAr)

            val nextExpr = {
              val pathInput = Apply(tapir("stringToPath"), List(Literal(StringConstant(name))))
              val select = Select.overloaded(expr, "in", List(TypeRepr.of[Unit], lr), List(pathInput))

              Apply(select, List(pc))
            }

            (nextExpr, lr, newArity)

          case PathSegment2.Subst(name) =>
            val input = pathInputs.find(_.input.name == name).get
            val (expr, prevLr, leftAr) = ewa
            val (lr, newArity, _, pc) = makeParamConcat(prevLr, input.input.tpe, leftAr)

            val nextExpr = {
              val pathInput = Select.overloaded(tapirInteropRoot, "path", List(input.input.tpe), List(Literal(StringConstant(input.input.name))))
              val select = Select.overloaded(expr, "in", List(input.input.tpe, lr), List(pathInput))

              Apply(select, List(pc))
            }

            (nextExpr, lr, newArity)
        }
      }

      val (withNonPathInputs, nonPathInputsLr, arityAfterNonPath) = nonPathInputs.foldLeft((withPathInputs, pathInputsLr, arityAfterPath)) { (ewa, input) =>
        val (expr, prevLr, leftAr) = ewa
        val (lr, newArity, _, pc) = makeParamConcat(prevLr, input.input.tpe, leftAr)

        val rightAr = if (input.input.tpe.typeSymbol == unitSym) 0 else 1

        val nextExpr = {
          val select = Select.overloaded(expr, "in", List(input.input.tpe, lr), List(input.term))

          Apply(select, List(pc))
        }

        (nextExpr, lr, newArity)
      }

      def inferOutputTypeEncoder(tpe: TypeRepr): Term =
        summon(typeApply(peo, tpe)) match {
          case None => mirror.defaultBody match {
            case Some(defBody) => summon(typeApply(eoc, tpe, defBodyToTag(defBody))) match {
              case None => errorAndAbort(s"Failed to find PEO or EOC for type $tpe")
              case Some(t) => Select.unique(t, "instance")
            }
            case None => errorAndAbort("Failed to find PEO and no default body present")
          }
          case Some(t) => Select.unique(t, "instance")
        }

      val withOutputs = method.output.out match {
        case Some(out) => {
          val (lr, _, _, pc) = makeParamConcat(TypeRepr.of[Unit], out.tpe, 0)

          Apply(Select.overloaded(withNonPathInputs, "out", List(out.tpe, lr), List(inferOutputTypeEncoder(out.tpe))), List(pc))
        }

        case None => withNonPathInputs
      }

      val withErrors = method.output.err match {
        case Some(err) => {
          val (lr, _, _, pc) = makeParamConcat(TypeRepr.of[Unit], err.tpe, 0)

          Apply(Select.overloaded(withOutputs, "errorOut", List(err.tpe, lr), List(inferOutputTypeEncoder(err.tpe))), List(pc))
        }

        case None => withOutputs
      }

      val realInputType = (pathInputs ++ nonPathInputs)
        .map(_.input.tpe)
        .filter { _.typeSymbol != unitSym }

      val inputTupleClass = defn.TupleClass(realInputType.size)
      val inputTupleType = Applied(TypeIdent(inputTupleClass), realInputType.map(ttree)).tpe

      val monadF = Expr.summon[Monad[F]].get.asTerm

      val requiredOutTpe = method.output.out.map(_.tpe).getOrElse(TypeRepr.of[Unit])
      val requiredErrTpe = method.output.err.map(_.tpe).getOrElse(TypeRepr.of[Unit])
      val requiredEitherTpe = typeApply(TypeRepr.of[Either[_, _]], requiredErrTpe, requiredOutTpe)
      val requiredResultType = typeApply(TypeRepr.of[F], requiredEitherTpe)

      val workerLambdaMType = MethodType(List("x"))(_ => List(inputTupleType), _ => requiredResultType)

      println()

      val workerLambda = Lambda(clsSym, workerLambdaMType, (sym, xs) => {
        val x = xs.head.symbol

        val args = indexedInputs.map {
          case IndexedInput(_, _, ix) =>
            Select.unique(Ref(x), "_" + (ix + 1))
        }

        val call = Apply(Select(clsExpr.asTerm, method.callTarget), args)

        def right(term: Term) =
          Apply(TypeApply(tapir("right"), List(requiredErrTpe, requiredOutTpe).map(ttree)), List(term))

        if (method.output.higher) {
          if (method.output.either) {
            call
          } else {
            val asRightLambdaTpe = MethodType(List("v"))(_ => List(requiredOutTpe), _ => requiredEitherTpe)

            Apply(Apply(Select.unique(monadF, "map"), List(call)), List(Lambda(clsSym, asRightLambdaTpe, (_, xs) => right(Ref(xs.head.symbol)))))
          }
        } else {
          if (method.output.either) {
            Apply(TypeApply(Select.unique(monadF, "pure"), List(ttree(requiredEitherTpe))), List(call))
          } else {
            val asRightLambdaTpe = MethodType(List("v"))(_ => List(requiredOutTpe), _ => requiredEitherTpe)

            Apply(TypeApply(Select.unique(monadF, "pure"), List(ttree(requiredEitherTpe))), List(right(call)))
          }
        }
      })

      Select.overloaded(withErrors, "serverLogic", List(TypeRepr.of[F]), List(workerLambda))
    }

    Expr.ofList(endpoints.map(_.asExprOf[S]))
  }
}
