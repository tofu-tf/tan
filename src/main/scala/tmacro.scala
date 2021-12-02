package tan

import scala.reflect.macros.blackbox
import scala.util.matching.Regex

object tmacro {
  private val camelCaseWordBoundary: Regex = "[A-Z\\d]".r

  def compileImpl[Cls: c.WeakTypeTag, F[_]](c: blackbox.Context)(cls: c.Expr[Cls])(implicit _f: c.WeakTypeTag[F[_]]): c.Tree = {
    sealed trait PathSegment
    object PathSegment {
      case class Raw(value: String) extends PathSegment
      case class Subst(name: String) extends PathSegment
    }

    sealed trait Input
    object Input {
      case class Path(name: String, tpe: c.Type) extends Input
      case class NonPath(tree: c.Tree) extends Input
      case class Security(sec: c.Tree) extends Input
    }

    sealed trait NamingConvention
    object NamingConvention {
      case object AsIs extends NamingConvention
      case object Snake extends NamingConvention
      case object SpaceSnake extends NamingConvention
      case object Kebab extends NamingConvention
    }

    case class NamingConventions(
      query: NamingConvention,
      pathSeg: NamingConvention
    )

    case class IndexedInput(input: Input, index: Int)

    import c.universe._

    val clsTpe = weakTypeOf[Cls]
    val fTpe = weakTypeOf[F[_]].typeConstructor.dealias

    val getAttrSym = symbolOf[get]
    val postAttrSym = symbolOf[post]
    val putAttrSym = symbolOf[put]
    val deleteAttrSym = symbolOf[delete]

    val methodSyms = Set[Symbol](getAttrSym, postAttrSym, putAttrSym, deleteAttrSym)

    val queryAttrSym = symbolOf[query]
    val bodyAttrSym = symbolOf[body]
    val securityAttrSym = symbolOf[security]

    val jsonAttrSym = symbolOf[json]
    val plainTextAttrSym = symbolOf[plainText]

    val eicType = weakTypeOf[EndpointInputConstructor[_, _]]

    def rename(name: String, conv: NamingConvention): String = {
      if (conv == NamingConvention.AsIs) return name

      val sep = conv match {
        case NamingConvention.Snake => "_"
        case NamingConvention.SpaceSnake => " "
        case NamingConvention.Kebab => "-"
        case _ => "?"
      }

      camelCaseWordBoundary.replaceAllIn(name, sep + _.group(0).toLowerCase())
    }

    val namingConventions = {
      val plainConvs = clsTpe.typeSymbol.annotations.map(a => c.typecheck(a.tree.duplicate)).collect {
        case q"new $attr()" if attr.tpe != null && attr.tpe.typeConstructor.typeSymbol == weakTypeOf[namingConvention[->[Named, tan.NamingConvention]]].typeConstructor.typeSymbol =>
          val List(n, v) = attr.tpe.typeArgs.head.typeArgs

          v.typeSymbol match {
            case x if x == symbolOf[snake] => n.typeSymbol -> NamingConvention.Snake
            case x if x == symbolOf[spaceSnake] => n.typeSymbol -> NamingConvention.SpaceSnake
            case x if x == symbolOf[kebab] => n.typeSymbol -> NamingConvention.Kebab
          }
      }

      NamingConventions(
        query = plainConvs.collectFirst { case (n, v) if n == symbolOf[query] => v }.getOrElse(NamingConvention.AsIs),
        pathSeg = plainConvs.collectFirst { case (n, v) if n == symbolOf[pathSeg] => v }.getOrElse(NamingConvention.AsIs),
      )
    }

    val groupTag = clsTpe.typeSymbol.annotations.map(a => c.typecheck(a.tree.duplicate)).collectFirst {
      case q"new $attr($tag)" if attr.tpe != null && attr.tpe.typeSymbol == weakTypeOf[tag].typeSymbol =>
        tag
    }

    val endpoints = clsTpe.decls.collect {
      case d if d.isMethod && d.isPublic && !d.isConstructor && d.asMethod.paramLists.size == 1 => d.asMethod
    }.map { d =>
      val defnName = d.name

      val summary = d.annotations.map(a => c.typecheck(a.tree.duplicate)).collectFirst {
        case q"new $attr($summary)" if attr.tpe != null && attr.tpe.typeSymbol == weakTypeOf[summary].typeSymbol =>
          summary
      }.getOrElse(Literal(Constant(rename(defnName.decodedName.toString, NamingConvention.SpaceSnake).capitalize)))

      val tag = d.annotations.map(a => c.typecheck(a.tree.duplicate)).collectFirst {
        case q"new $attr($tag)" if attr.tpe.typeSymbol == weakTypeOf[tag].typeSymbol =>
          tag
      }

      val paths = d.annotations.map(_.tree).collect {
        case q"new $attr($path)" if methodSyms.contains(attr.symbol) =>
          path match {
            case Literal(Constant(c: String)) =>
              val methodName = attr.symbol match {
                case `getAttrSym` => TermName("get")
                case `postAttrSym` => TermName("post")
                case `putAttrSym` => TermName("put")
                case `deleteAttrSym` => TermName("delete")
              }

              c -> methodName
            case _ => c.abort(c.enclosingPosition, "Non-literal path value")
          }
      }

      val (rawPath, methodName) = paths match {
        case List(path) => path
        case Nil => c.abort(c.enclosingPosition, "Path was not defined")
        case _ => c.abort(c.enclosingPosition, "Path was defined more than once")
      }

      val segmentedPath = rawPath.split('/').map {
        case s"{$name}" => PathSegment.Subst(name)
        case s => PathSegment.Raw(s)
      }

      val defBody =  clsTpe.typeSymbol.annotations.map(a => c.typecheck(a.tree.duplicate)).collectFirst {
        case tree if tree.tpe.typeConstructor.typeSymbol == weakTypeOf[defaultBody[DefaultBody]].typeConstructor.typeSymbol =>
          tree.tpe.typeArgs.head match {
            case t if t.typeSymbol == symbolOf[json] => weakTypeOf[DefTags.JsonDefTag]
            case t => c.abort(c.enclosingPosition, s"Unknown defType $t")
          }
      }

      val inputs = d.paramLists.head.map { param =>
        val paramTpe = param.typeSignature
        val paramTpeDealiased = param.typeSignature.dealias
        val paramName = param.name.decodedName.toString

        val inputAnnotations: List[Input] = param.annotations.map(_.tree).collect {
          case q"new $attr" if attr.symbol == queryAttrSym => Input.NonPath(q"_root_.sttp.tapir.query[$paramTpe](${rename(paramName, namingConventions.query)})")
          case q"new $attr" if attr.symbol == bodyAttrSym =>
            val explicitBodyTypeAnnot = param.annotations.map(_.tree).collectFirst {
              case q"new $attr" if attr.symbol == jsonAttrSym =>
                c.inferImplicitValue(appliedType(eicType, paramTpeDealiased, weakTypeOf[DefTags.JsonDefTag])) match {
                  case EmptyTree => c.abort(c.enclosingPosition, s"Failed to find PEI or EIC for type $paramTpe (dealiased to $paramTpeDealiased) (note: this is explicit json body)")
                  case t => Input.NonPath(q"$t.instance")
                }
              case q"new $attr" if attr.symbol == plainTextAttrSym =>
                if (paramTpeDealiased == typeOf[String].dealias) {
                  Input.NonPath(q"_root_.sttp.tapir.plainBody[String]")
                } else {
                  c.abort(c.enclosingPosition, s"Invalid/unsupported type for plainBody input: $paramTpeDealiased (expecting ${weakTypeOf[String]} or ${typeOf[String]})")
                }
            }

            explicitBodyTypeAnnot match {
              case Some(ex) => ex
              case None =>
                c.inferImplicitValue(appliedType(weakTypeOf[ProvidedEndpointInput[_]].typeConstructor, paramTpeDealiased)) match {
                  case EmptyTree => defBody match {
                    case Some(defBody) => c.inferImplicitValue(appliedType(eicType, paramTpeDealiased, defBody)) match {
                      case EmptyTree => c.abort(c.enclosingPosition, s"Failed to find PEI or EIC for type $paramTpe (dealiased to $paramTpeDealiased), also tried to check explicit annots")
                      case t => Input.NonPath(q"$t.instance")
                    }
                    case None => c.abort(c.enclosingPosition, s"Failed to find PEI and no defaultBody was found")
                  }
                  case t => Input.NonPath(q"$t.instance")
                }
            }
          case q"new $attr" if attr.symbol == securityAttrSym => c.inferImplicitValue(appliedType(weakTypeOf[Security[_, _, F]].typeConstructor, WildcardType, paramTpe, fTpe)) match {
            case EmptyTree => c.abort(c.enclosingPosition, s"Failed to find Security instance for type $paramTpe")
            case sec => Input.Security(sec)
          }
        }

        inputAnnotations match {
          case List(input) => input
          case Nil => Input.Path(paramName, paramTpe)
          case _ => c.abort(c.enclosingPosition, "Input type was defined more than once")
        }
      }

      if (segmentedPath.collect { case PathSegment.Subst(n) => n }.toSet != inputs.collect { case Input.Path(name, _) => name }.toSet) {
        c.abort(c.enclosingPosition, "Path segments and path arguments do not match")
      }

      val indexedInputs = {
        val pathInputs = inputs.collect { case p: Input.Path => p }
        val nonPathInputs = inputs.collect { case np: Input.NonPath => np }
        val secInputs = inputs.collect { case np: Input.Security => np }

        if (secInputs.size > 1) {
          c.abort(c.enclosingPosition, "Multiple secure endpoints are not supported now")
        }

        val indexedPathInputs = pathInputs.map(p => IndexedInput(p, segmentedPath.collect { case s: PathSegment.Subst => s }.indexWhere(_.name == p.name)))
        val indexedNonPathInputs = nonPathInputs.zipWithIndex.map { case (np, i) => IndexedInput(np, i + indexedPathInputs.size) }
        val indexedSecInputs = secInputs.zipWithIndex.map { case (np, i) => IndexedInput(np, -1) }

        val indexedInputsUnordered = indexedPathInputs ++ indexedSecInputs ++ indexedNonPathInputs

        inputs.map { i => indexedInputsUnordered.find(_.input == i).get }
      }

      case class DecodedOutType(eo: Tree)

      case class DecodedOutTypes(
        async: Boolean,
        isEither: Boolean,
        outType: Option[DecodedOutType],
        errType: Option[DecodedOutType]
      )

      val outputType = {
        def decodeOutputType(tpe: Type, async: Boolean): DecodedOutTypes = {
          def decodeExactOutputType(tpe: Type): Option[DecodedOutType] = {
            val dealiased = tpe.dealias

            if (dealiased.typeSymbol == symbolOf[Unit]) return None

            c.inferImplicitValue(appliedType(weakTypeOf[ProvidedEndpointOutput[_]].typeConstructor, dealiased)) match {
              case EmptyTree => defBody match {
                case Some(defBody) => c.inferImplicitValue(appliedType(weakTypeOf[EndpointOutputConstructor[_, _]], dealiased, defBody)) match {
                  case EmptyTree => c.abort(c.enclosingPosition, s"Failed to find PEO or EOC for type $tpe (dealiased to $dealiased)")
                  case t => Some(DecodedOutType(q"$t.instance"))
                }
                case None => c.abort(c.enclosingPosition, s"Failed to find PEO and no defaultBody was found")
              }
              case t => Some(DecodedOutType(q"$t.instance"))
            }
          }

          if (tpe.typeConstructor.typeSymbol == weakTypeOf[Either[_, _]].typeConstructor.typeSymbol) {
            val List(left, right) = tpe.typeArgs

            DecodedOutTypes(async = async, isEither = true, outType = decodeExactOutputType(right), errType = decodeExactOutputType(left))
          } else {
            DecodedOutTypes(async = async, isEither = false, outType = decodeExactOutputType(tpe), errType = None)
          }
        }

        d.returnType.typeConstructor.dealias match {
          case tc if tc.takesTypeArgs && tc.typeSymbol == fTpe.typeSymbol =>
            if (tc.typeParams.size == 1)
              decodeOutputType(d.returnType.typeArgs.head, async = true)
            else
              c.abort(c.enclosingPosition, "Multi-arg effect type constructors are not supported now")
          case _ =>
            decodeOutputType(d.returnType, async = false)
        }
      }

      def tupleMappingTree(hasSec: Boolean, multiInput: Boolean) = {
        val args = if (hasSec)
          indexedInputs.map {
            case IndexedInput(_, -1) =>  q"t._1"
            case IndexedInput(_, ix) =>
              if (multiInput)
                q"t._2.${TermName("_" + (ix + 1))}"
              else
                q"t._2"
          }
        else if (multiInput)
          indexedInputs.map(i => q"t.${TermName("_" + (i.index + 1))}")
        else
          indexedInputs.map(i => q"t")

        val rawBody = q"$cls.$defnName(..$args)"
        val monadForF = q"_root_.cats.Monad[$fTpe]"

        val wrappedBody = outputType match {
          case DecodedOutTypes(true, true, _, _) => rawBody
          case DecodedOutTypes(true, false, _, _) => q"monadF.fmap($rawBody)(_.asRight)"
          case DecodedOutTypes(false, true, _, _) => q"monadF.pure($rawBody)"
          case DecodedOutTypes(false, false, _, _) => q"monadF.pure($rawBody.asRight)"
        }

        q"{ import _root_.cats.syntax.either._; val monadF = $monadForF; t => $wrappedBody }"
      }

      val pathInputs = inputs.collect { case p: Input.Path => p }
      val secInputs = inputs.collect { case np: Input.Security => np }
      val nonPathInputs = inputs.collect { case np: Input.NonPath => np }

      val endpointDef = {
        val pathNodes = segmentedPath.map {
          case PathSegment.Raw(r) =>
            Literal(Constant(r))
          case PathSegment.Subst(s) =>
            val tpe = pathInputs.find(p => p.name == s).get.tpe
            q"_root_.sttp.tapir.path[$tpe](${rename(s, namingConventions.pathSeg)})"
        }

        val pathInput = pathNodes.reduce((l, r) => q"$l / $r")

        val endpointWithMethod: Tree = {
          val withMethodAndSummary = q"endpoint.$methodName.summary($summary)"
          val withGroupTag = groupTag match {
            case Some(gtag) => q"$withMethodAndSummary.tag($gtag)"
            case None => withMethodAndSummary
          }
          val withDefnTag = tag match {
            case Some(gtag) => q"$withGroupTag.tag($gtag)"
            case None => withGroupTag
          }

          withDefnTag
        }

        val withInputs = {
          val withSecInputs = secInputs.foldLeft(endpointWithMethod)((e, i) => q"$e.in(${i.sec}.input).serverLogicForCurrent(${i.sec}.handler)")
          val withPath = q"$withSecInputs.in($pathInput)"

          nonPathInputs.foldLeft(withPath)((e, i) => q"$e.in(${i.tree})")
        }

        val withOutputs = outputType.outType match {
          case Some(out) => q"$withInputs.out(${out.eo})"
          case None => withInputs
        }

        val withErrors = outputType.errType match {
          case Some(err) => q"$withOutputs.errorOut(${err.eo})"
          case None => withOutputs
        }

        withErrors
      }

      val hasSec = secInputs.nonEmpty
      val multiInput = (pathInputs.size + nonPathInputs.size) > 1

      if (hasSec)
        q"$endpointDef.serverLogic(${tupleMappingTree(hasSec, multiInput)})"
      else
        q"$endpointDef.serverLogic[$fTpe](${tupleMappingTree(hasSec, multiInput)})"
    }

    q"List(..$endpoints)"
  }
}
