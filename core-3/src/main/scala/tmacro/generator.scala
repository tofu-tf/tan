package tan
package tmacro

import tmacro.mirror1.{HttpMethod1, ControllerMirror1, AnnotationMirror1, DefaultBodyMirror1}
import tmacro.mirror2.{ControllerMirror2, MethodInput2, NamingConvention2, PathSegment2}

import scala.quoted.*
import attrs.{tapirVersion, DefTags}

import scala.util.matching.Regex

class generator[Q <: Quotes](using val q: Q) {
  import q.reflect.*
  import q.reflect.report.*

  object types {
    val unit = TypeRepr.of[Unit]
    val string = TypeRepr.of[String]
  }

  val placeholder = '{ ??? }.asTerm

  def ttree(repr: TypeRepr): TypeTree = TypeTree.of(using repr.asType)

  def mkTuple(types: TypeRepr*): TypeRepr = mkTuple(types.toList)
  def mkTuple(types: List[TypeRepr]): TypeRepr = {
    val tupleClass = defn.TupleClass(types.size)

    Applied(TypeIdent(tupleClass), types.map(ttree)).tpe
  }

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

  val fakeParamConcat = TypeIdent(Symbol.requiredClass("tan.TapirScalaMacrosInterop.FakeParamConcat")).tpe
  val paramConcat = TypeIdent(Symbol.requiredClass("sttp.tapir.typelevel.ParamConcat.Aux")).tpe

  def makeParamConcat(l: TypeRepr, r: TypeRepr, leftAr: Int): (TypeRepr, Int, TypeRepr, Term) = {
    val rightAr = if (r.typeSymbol == types.unit.typeSymbol) 0 else 1

    val lr = if (rightAr == 0) l else leftAr match {
      case 0 => r
      case 1 => mkTuple(l, r)
      case _ =>
        val AppliedType(_, ls) = l
        mkTuple(ls :+ r)
    }

    val concat = Apply(TypeApply(Select.unique(New(ttree(typeApply(fakeParamConcat, l, r, lr))), "<init>"), List(l, r, lr).map(ttree)), List(Literal(IntConstant(leftAr)), Literal(IntConstant(rightAr))))

    (lr, leftAr + rightAr, typeApply(paramConcat, l, r, lr), concat)
  }

  case class SecurityInput(
    desc: MethodInput2[TypeRepr],
    inputTpe: TypeRepr,
    underlyingInputTpe: TypeRepr,
    errorTpe: TypeRepr,

    underlyingInput: Term,
    errorOutput: Term,
    handler: Term
  )

  def apply[Cls: Type, F[_]: Type, S: Type](clsExpr: Expr[Cls], mirror: ControllerMirror2[q.reflect.Symbol, q.reflect.TypeRepr]): Expr[List[S]] = {

    case class InputWithTree[I <: MethodInput2[TypeRepr]](input: I, term: Term)
    case class IndexedInput(input: MethodInput2[TypeRepr], index: Int)

    val clsSym = TypeRepr.of[Cls].typeSymbol

    val tapirInteropRoot: Term = Ref(Symbol.requiredModule("tan.TapirScalaMacrosInterop"))
    def tapir(name: String): Term = Select.unique(tapirInteropRoot, name)

    val ctrType = TypeIdent(Symbol.requiredClass("tan.Controller")).tpe
    val tapirVersionSym = TypeRepr.of[tapirVersion].typeSymbol

    val secType = TypeIdent(Symbol.requiredClass("tan.Security")).tpe

    val eic = TypeIdent(Symbol.requiredClass("tan.EndpointInputConstructor")).tpe
    val eoc = TypeIdent(Symbol.requiredClass("tan.EndpointOutputConstructor")).tpe
    val pei = TypeIdent(Symbol.requiredClass("tan.ProvidedEndpointInput")).tpe
    val peo = TypeIdent(Symbol.requiredClass("tan.ProvidedEndpointOutput")).tpe

    val tapirVersion = ctrType.typeSymbol.annotations.collectFirst {
      case annot if annot.tpe.typeSymbol == tapirVersionSym =>
        val Apply(_, List(Literal(StringConstant(tapirVersion)))) = annot
        tapirVersion
    }.get

    val useNewSecurity = tapirVersion match {
      case "18" => false
      case "19" => true
      case v => errorAndAbort("Unsupported tapir version " + v)
    }

    val catsMonad = TypeIdent(Symbol.requiredClass("cats.Monad")).tpe

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
          Literal(StringConstant(method.summary.getOrElse(renamer.rename(method.name, NamingConvention2.SpaceSnake).capitalize)))
        )
      )

      val pathInputs = method.inputs.collect {
        case p: MethodInput2.Path[TypeRepr] => InputWithTree(p, placeholder)
      }

      def defBodyToTag(defBody: DefaultBodyMirror1): TypeRepr =
        defBody match {
          case AnnotationMirror1.Json => TypeRepr.of[DefTags.JsonDefTag]
          case AnnotationMirror1.PlainText => TypeRepr.of[DefTags.PlainTextDefTag]
        }

      def summonEio(tpe: TypeRepr, out: Boolean, defBody: Option[DefaultBodyMirror1]): Term = {
        val dealiased = tpe.dealias

        val eioc = if (out) eoc else eic
        val peio = if (out) peo else pei
        val what = if (out) "PEO or EOC" else "PEI or EIC"

        defBody match {
          case Some(defBody) =>
            summon(typeApply(eioc, dealiased, defBodyToTag(defBody))) match {
              case None => errorAndAbort(s"Failed to find $what for type $tpe (dealiased to $dealiased) (note: this is explicit json body)")
              case Some(eic) => Select.unique(eic, "instance")
            }
          case None =>
            summon(typeApply(peio, dealiased)) match {
              case None => mirror.defaultBody match {
                case Some(defBody) => summon(typeApply(eioc, dealiased, defBodyToTag(defBody))) match {
                  case None => errorAndAbort(s"Failed to find $what for type $tpe (dealiased to $dealiased) (note: checked default body)")
                  case Some(t) => Select.unique(t, "instance")
                }
                case None => errorAndAbort(s"Failed to find $what for type $tpe (dealiased to $dealiased) (note: no default body present)")
              }
              case Some(t) => Select.unique(t, "instance")
            }
        }
      }

      val nonPathInputs = method.inputs.collect {
        case np: MethodInput2.Query[TypeRepr] =>
          val tree = Apply(TypeApply(tapir("query"), List(ttree(np.tpe))), List(Literal(StringConstant(np.name))))

          InputWithTree(np, tree)
        case np: MethodInput2.Body[TypeRepr] =>
          InputWithTree(np, summonEio(np.tpe, false, np.bodyType))
      }

      val secInput = {
        val inputs = method.inputs.collect {
          case np: MethodInput2.Security[TypeRepr] =>
            summon(typeApply(secType, np.tpe, TypeRepr.of[F])) match {
              case None => errorAndAbort(s"Failed to find Security instance for type ${np.tpe}")
              case Some(secRaw) =>
                SecurityInput(
                  desc = np,
                  inputTpe = np.tpe,
                  underlyingInputTpe = TypeSelect(secRaw, "VIn").tpe,
                  errorTpe = TypeSelect(secRaw, "VErr").tpe,

                  underlyingInput = Select.unique(secRaw, "input"),
                  errorOutput = Select.unique(secRaw, "errorOutput"),
                  handler = Select.unique(secRaw, "handler"),
                )
            }
        }

        if (inputs.size > 1)
          errorAndAbort("Multiple secure inputs are not supported")

        inputs.headOption
      }

      val indexedInputs = {
        val indexedPathInputs = pathInputs.map { p =>
          IndexedInput(
            input = p.input,
            index = method.pathSegments.collect { case s: PathSegment2.Subst => s }.indexWhere(_.name == p.input.name)
          )
        }

        val indexedNonPathInputs = nonPathInputs.zipWithIndex.map {
          case (np, i) => IndexedInput(np.input, i + indexedPathInputs.size)
        }

        val indexedSecInput = secInput.map { np =>
          IndexedInput(np.desc, -1)
        }

        val indexedInputsUnordered = indexedPathInputs ++ indexedSecInput ++ indexedNonPathInputs

        method.inputs.map { i => indexedInputsUnordered.find(_.input == i).get }
      }

      val (withErrors, totalErrorOut) = method.output.err match {
        case Some(err) =>
          val (errTpe, encoder, lr, pc) = secInput match {
            case Some(secInput) =>
              val logicErrEncoder = summonEio(err.tpe, true, None)

              val eitherEncoder = Apply(
                TypeApply(tapir("eitherError"), List(ttree(err.tpe), ttree(secInput.errorTpe))),
                List(
                  logicErrEncoder,
                  secInput.errorOutput
                )
              )

              val eitherTpe = typeApply(TypeRepr.of[Either[_, _]], err.tpe, secInput.errorTpe)

              val (lr, _, _, pc) = makeParamConcat(TypeRepr.of[Unit], eitherTpe, 0)

              (eitherTpe, eitherEncoder, lr, pc)
            case None =>
              val errTpe = err.tpe
              val (lr, _, _, pc) = makeParamConcat(TypeRepr.of[Unit], errTpe, 0)
              val encoder = summonEio(errTpe, true, None)

              (errTpe, encoder, lr, pc)
          }

          (Apply(Select.overloaded(endpointBase, "errorOut", List(errTpe, lr), List(encoder)), List(pc)), errTpe)

        case None =>
          secInput match {
            case Some(secInput) =>
              val eitherEncoder = Apply(
                TypeApply(tapir("onlySecError"), List(ttree(secInput.errorTpe))),
                List(
                  secInput.errorOutput
                )
              )

              val eitherTpe = typeApply(TypeRepr.of[Either[_, _]], TypeRepr.of[Unit], secInput.errorTpe)

              val (lr, _, _, pc) = makeParamConcat(TypeRepr.of[Unit], eitherTpe, 0)

              (Apply(Select.overloaded(endpointBase, "errorOut", List(eitherTpe, lr), List(eitherEncoder)), List(pc)), eitherTpe)
            case None => (endpointBase, TypeRepr.of[Unit])
          }
      }

      val monadF = summon(typeApply(catsMonad, TypeRepr.of[F])).get

      val requiredOutTpe = method.output.out.map(_.tpe).getOrElse(TypeRepr.of[Unit])
      val requiredErrTpe = method.output.err.map(_.tpe).getOrElse(TypeRepr.of[Unit])
      val requiredEitherTpe = typeApply(TypeRepr.of[Either[_, _]], requiredErrTpe, requiredOutTpe)

      val withSecInput = secInput match {
        case Some(secInput) =>
          val errorOrOut = typeApply(TypeRepr.of[Either[_, _]], secInput.errorTpe, secInput.inputTpe)
          val errorsOrOut = typeApply(TypeRepr.of[Either[_, _]], typeApply(TypeRepr.of[Either[_, _]], requiredErrTpe, secInput.errorTpe), secInput.inputTpe)
          val secHandlerResultTpe = typeApply(TypeRepr.of[F], errorsOrOut)

          val secHandlerTermWrappedMType = MethodType(List("i"))(_ => List(secInput.underlyingInputTpe), _ => secHandlerResultTpe)
          val secHandlerTermWrapped = Lambda(Symbol.noSymbol, secHandlerTermWrappedMType, (sym, xs) => {
            val secHandlerTerm = secInput.handler.etaExpand(sym)
            val call = Apply(Select.unique(secHandlerTerm, "apply"), List(Ref(xs.head.symbol)))
            val asRightLambdaTpe = MethodType(List("v"))(_ => List(errorOrOut), _ => errorsOrOut)

            def leftWrap(term: Term) = Apply(TypeApply(tapir("leftWrap"), List(secInput.errorTpe, requiredErrTpe, secInput.inputTpe).map(ttree)), List(term))

            Apply(Apply(TypeApply(Select.unique(monadF, "map"), List(ttree(errorOrOut), ttree(errorsOrOut))), List(call)), List(Lambda(sym, asRightLambdaTpe, (_, xs) => leftWrap(Ref(xs.head.symbol)))))
          })

          val (lr, _, _, pc) = makeParamConcat(TypeRepr.of[Unit], secInput.underlyingInputTpe, 0)

          val (secInMethodName, secLogicMethodName) = if (useNewSecurity) {
            ("securityIn", "serverSecurityLogic")
          } else {
            ("in", "serverLogicForCurrent")
          }

          val withSecIn = Apply(Select.overloaded(withErrors, secInMethodName, List(secInput.underlyingInputTpe, lr), List(secInput.underlyingInput)), List(pc))

          Apply(TypeApply(Select.unique(withSecIn, secLogicMethodName), List(ttree(secInput.inputTpe), TypeTree.of[F])), List(secHandlerTermWrapped))

        case None =>
          withErrors
      }

      val (withPathInputs, pathInputsLr, arityAfterPath) = method.pathSegments.foldLeft((withSecInput, TypeRepr.of[Unit], 0)) { (ewa, segment) =>
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

        val rightAr = if (input.input.tpe.typeSymbol == types.unit.typeSymbol) 0 else 1

        val nextExpr = {
          val select = Select.overloaded(expr, "in", List(input.input.tpe, lr), List(input.term))

          Apply(select, List(pc))
        }

        (nextExpr, lr, newArity)
      }

      val withOutputs = method.output.out match {
        case Some(out) => {
          val (lr, _, _, pc) = makeParamConcat(TypeRepr.of[Unit], out.tpe, 0)

          Apply(Select.overloaded(withNonPathInputs, "out", List(out.tpe, lr), List(summonEio(out.tpe, true, None))), List(pc))
        }

        case None => withNonPathInputs
      }

      val realInputType = (pathInputs ++ nonPathInputs)
        .map(_.input.tpe)
        .filter { _.typeSymbol != types.unit.typeSymbol }

      val requiredResultType =
        secInput match {
          case Some(secInput) =>
            val errorsOrOut = typeApply(TypeRepr.of[Either[_, _]], typeApply(TypeRepr.of[Either[_, _]], requiredErrTpe, secInput.errorTpe), requiredOutTpe)

            typeApply(TypeRepr.of[F], errorsOrOut)
          case None =>
            typeApply(TypeRepr.of[F], requiredEitherTpe)
        }

      val workerLambda = {
        val innerTupleType = if (realInputType.isEmpty) {
          types.unit
        } else if (realInputType.size == 1) {
          realInputType.head
        } else {
          mkTuple(realInputType)
        }

        def innerLambdaBody(outerParamTree: Term, innerParamTree: Term, lamSym: Symbol) = {
          val hasMultipleNonSecInputs = realInputType.size > 1

          val args = indexedInputs.map {
            case IndexedInput(_, -1) =>
              outerParamTree
            case IndexedInput(_, ix) =>
              if (hasMultipleNonSecInputs)
                Select.unique(innerParamTree, "_" + (ix + 1))
              else
                innerParamTree
          }

          val call = Apply(Select(clsExpr.asTerm, method.callTarget), args)

          def right(term: Term) =
            Apply(TypeApply(tapir("right"), List(requiredErrTpe, requiredOutTpe).map(ttree)), List(term))

          val coercedToIEO = if (method.output.higher) {
            if (method.output.either) {
              call
            } else {
              val asRightLambdaTpe = MethodType(List("v"))(_ => List(requiredOutTpe), _ => requiredEitherTpe)

              Apply(Apply(TypeApply(Select.unique(monadF, "map"), List(ttree(requiredOutTpe), ttree(requiredEitherTpe))), List(call)), List(Lambda(lamSym, asRightLambdaTpe, (_, xs) => right(Ref(xs.head.symbol)))))
            }
          } else {
            if (method.output.either) {
              Apply(TypeApply(Select.unique(monadF, "pure"), List(ttree(requiredEitherTpe))), List(call))
            } else {
              Apply(TypeApply(Select.unique(monadF, "pure"), List(ttree(requiredEitherTpe))), List(right(call)))
            }
          }

          secInput match {
            case Some(secInput) =>
              Apply(
                TypeApply(tapir("coerceResultToSecure"), List(TypeTree.of[F], ttree(requiredErrTpe), ttree(secInput.errorTpe), ttree(requiredOutTpe))),
                List(monadF, coercedToIEO)
              )
            case None => coercedToIEO
          }
        }

        val innerLambdaMType = (secInput, useNewSecurity) match {
          case (None, _) | (Some(_), true) =>
            MethodType(List("x"))(_ => List(innerTupleType), _ => requiredResultType)
          case (Some(secInput), _) =>
            val outerTupleType = mkTuple(secInput.inputTpe, innerTupleType)

            MethodType(List("x"))(_ => List(outerTupleType), _ => requiredResultType)
        }

        secInput match {
          case Some(secInput) =>
            if (useNewSecurity) {
              val innerFunctionClass = defn.FunctionClass(1, false, false)
              val innerFunctionType = Applied(TypeIdent(innerFunctionClass), List(innerTupleType, requiredResultType).map(ttree)).tpe
              val outerLambdaMType = MethodType(List("x"))(_ => List(secInput.inputTpe), _ => innerFunctionType)

              Lambda(Symbol.noSymbol, outerLambdaMType, (sym, oxs) => {
                Lambda(sym, innerLambdaMType, (sym, ixs) => innerLambdaBody(Ref(oxs.head.symbol), Ref(ixs.head.symbol), sym))
              })
            } else {
              Lambda(Symbol.noSymbol, innerLambdaMType, (sym, ixs) => {
                val ref = Ref(ixs.head.symbol)
                innerLambdaBody(Select.unique(ref, "_1"), Select.unique(ref, "_2"), sym)
              })
            }
          case None =>
            Lambda(Symbol.noSymbol, innerLambdaMType, (sym, xs) => innerLambdaBody(placeholder, Ref(xs.head.symbol), sym))
        }
      }

      if (secInput.nonEmpty) {
        Select.overloaded(withOutputs, "serverLogic", Nil, List(workerLambda))
      } else if (useNewSecurity) {
        val unitIsUnit = '{implicitly[Unit =:= Unit]}.asTerm

        Apply(Select.overloaded(withOutputs, "serverLogic", List(TypeRepr.of[F]), List(workerLambda)), List(unitIsUnit))
      } else {
        Select.overloaded(withOutputs, "serverLogic", List(TypeRepr.of[F]), List(workerLambda))
      }
    }

    Expr.ofList(endpoints.map(_.asExprOf[S]))
  }
}
