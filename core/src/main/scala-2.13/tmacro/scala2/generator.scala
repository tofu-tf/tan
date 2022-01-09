package tan
package tmacro
package scala2

import scala.reflect.macros.blackbox
import scala.util.matching.Regex
import attrs._

import mirror1._
import mirror2._

object generator {
  def apply[C <: blackbox.Context, Cls: c.WeakTypeTag, F[_]](c: C)(mirror: ControllerMirror2[c.universe.Tree, c.universe.Type])(implicit _f: c.WeakTypeTag[F[_]]): c.Tree = {
    import c.universe._

    case class InputWithTree[I <: MethodInput2[Type]](input: I, tree: c.Tree)
    case class IndexedInput(input: MethodInput2[Type], tree: c.Tree, index: Int)

    val fTpe = weakTypeOf[F[_]].typeConstructor

    def findType(tree: Tree): Type = c.typecheck(tree).tpe

    val ctrType = findType(q"(???): _root_.tan.Controller[({type L[A] = A})#L]")
    val eicType = findType(q"(???): _root_.tan.EndpointInputConstructor[_, _]")
    val eocType = findType(q"(???): _root_.tan.EndpointOutputConstructor[_, _]")
    val peiType = findType(q"(???): _root_.tan.ProvidedEndpointInput[_]")
    val peoType = findType(q"(???): _root_.tan.ProvidedEndpointOutput[_]")
    val secType = findType(q"(???): _root_.tan.Security[_, ({type L[A] = A})#L]")

    val tapirVersion = ctrType.typeSymbol.annotations.map(_.tree).collectFirst {
      case q"new $annot($arg)" if annot.symbol == symbolOf[tapirVersion] =>
        arg match {
          case Literal(Constant(s: String)) => s
        }
    }.get

    val useNewSecurity = tapirVersion match {
      case "18" => false
      case "19" => true
      case v => c.abort(c.enclosingPosition, "Unsupported tapir version " + v)
    }

    val endpoints = mirror.methods.map { method =>
      val pathInputs = method.inputs.collect {
        case p: MethodInput2.Path[Type] => InputWithTree(p, EmptyTree)
      }

      def defBodyToTag(defBody: DefaultBodyMirror1): Type =
        defBody match {
          case AnnotationMirror1.Json => weakTypeOf[DefTags.JsonDefTag]
        }

      val nonPathInputs = method.inputs.collect {
        case np: MethodInput2.Query[Type] =>
          InputWithTree(np, q"_root_.sttp.tapir.query[${np.tpe}](${renamer.rename(np.name, mirror.namingConventions.query)})")
        case np: MethodInput2.Body[Type] =>
          val dealiased = np.tpe.dealias

          np.bodyType match {
            case Some(AnnotationMirror1.Json) =>
              c.inferImplicitValue(appliedType(eicType, dealiased, weakTypeOf[DefTags.JsonDefTag])) match {
                case EmptyTree => c.abort(c.enclosingPosition, s"Failed to find PEI or EIC for type ${np.tpe} (dealiased to $dealiased) (note: this is explicit json body)")
                case t => InputWithTree(np, q"$t.instance")
              }
            case Some(AnnotationMirror1.PlainText) =>
              if (dealiased == typeOf[String].dealias) {
                InputWithTree(np, q"_root_.sttp.tapir.plainBody[String]")
              } else {
                c.abort(c.enclosingPosition, s"Invalid/unsupported type for plainBody input: $dealiased (expecting String)")
              }
            case None =>
              c.inferImplicitValue(appliedType(peiType.typeConstructor, dealiased)) match {
                case EmptyTree => mirror.defaultBody match {
                  case Some(defBody) => c.inferImplicitValue(appliedType(eicType, dealiased, defBodyToTag(defBody))) match {
                    case EmptyTree => c.abort(c.enclosingPosition, s"Failed to find PEI or EIC for type ${np.tpe} (dealiased to $dealiased), also tried to check explicit annots")
                    case t => InputWithTree(np, q"$t.instance")
                  }
                  case None => c.abort(c.enclosingPosition, s"Failed to find PEI and no defaultBody was found")
                }
                case t => InputWithTree(np, q"$t.instance")
              }
          }
      }

      val secInputs = method.inputs.collect {
        case np: MethodInput2.Security[Type] =>
          c.inferImplicitValue(appliedType(secType.typeConstructor, np.tpe, fTpe)) match {
            case EmptyTree => c.abort(c.enclosingPosition, s"Failed to find Security instance for type ${np.tpe}")
            case sec => InputWithTree(np, sec)
          }
      }
      val secInput = if (secInputs.size > 1) {
        c.abort(c.enclosingPosition, "Multiple secure endpoints are not supported now")
      } else {
        secInputs.headOption
      }

      val indexedInputs = {
        val indexedPathInputs = pathInputs.map { p =>
          IndexedInput(
            input = p.input,
            tree = EmptyTree,
            index = method.pathSegments.collect { case s: PathSegment2.Subst => s }.indexWhere(_.name == p.input.name)
          )
        }

        val indexedNonPathInputs = nonPathInputs.zipWithIndex.map {
          case (np, i) => IndexedInput(np.input, np.tree, i + indexedPathInputs.size)
        }

        val indexedSecInput = secInput.map { np =>
          IndexedInput(np.input, np.tree, -1)
        }

        val indexedInputsUnordered = indexedPathInputs ++ indexedSecInput ++ indexedNonPathInputs

        method.inputs.map { i => indexedInputsUnordered.find(_.input == i).get }
      }

      val monadForF = q"_root_.cats.Monad[$fTpe]"

      def tupleMappingTree(hasSec: Boolean, multiInput: Boolean) = {
        val args = if (hasSec) {
          indexedInputs.map {
            case IndexedInput(_, _, -1) => q"t._1"
            case IndexedInput(_, _, ix) =>
              if (multiInput)
                q"t._2.${TermName("_" + (ix + 1))}"
              else
                q"t._2"
          }
        } else if (multiInput)
          indexedInputs.map(i => q"t.${TermName("_" + (i.index + 1))}")
        else
          indexedInputs.map(i => q"t")

        val rawBody = q"${method.callTarget}(..$args)"

        val wrappedBody = method.output match {
          case MethodOutput2(true, true, _, _) => rawBody
          case MethodOutput2(true, false, _, _) => q"monadF.fmap($rawBody)(_.asRight)"
          case MethodOutput2(false, true, _, _) => q"monadF.pure($rawBody)"
          case MethodOutput2(false, false, _, _) => q"monadF.pure($rawBody.asRight)"
        }

        val secErr = secInput.map(x => c.typecheck(x.tree.duplicate).tpe.member(TypeName("VErr")).typeSignature).getOrElse(typeOf[Unit])

        val resultLambda = if (hasSec && useNewSecurity) {
          q"(s => { i => { val t = (s, i); monadF.fmap($wrappedBody)(x => x.left.map(l => Left(l))) } })"
        } else if (hasSec) {
          q"(t => monadF.fmap($wrappedBody)(x => x.left.map(l => Left(l))))"
        } else {
          q"(t => $wrappedBody)"
        }

        q"{ import _root_.cats.syntax.either._; val monadF = $monadForF; $resultLambda }"
      }

      val endpointDef = {
        val pathNodes = method.pathSegments.map {
          case PathSegment2.Raw(r) =>
            Literal(Constant(r))
          case PathSegment2.Subst(s) =>
            val tpe = pathInputs.find(p => p.input.name == s).get.input.tpe
            q"_root_.sttp.tapir.path[$tpe](${renamer.rename(s, mirror.namingConventions.pathSeg)})"
        }

        val pathInput = pathNodes.reduce((l, r) => q"$l / $r")

        val endpointWithMethod: Tree = {
          val summary = method.summary.getOrElse(renamer.rename(method.name, NamingConvention2.SpaceSnake).capitalize)
          val methodName = method.method match {
            case HttpMethod1.Get => TermName("get")
            case HttpMethod1.Post => TermName("post")
            case HttpMethod1.Put => TermName("put")
            case HttpMethod1.Delete => TermName("delete")
          }

          val withMethodAndSummary = q"endpoint.$methodName.summary($summary)"
          val withGroupTag = mirror.tag match {
            case Some(gtag) => q"$withMethodAndSummary.tag($gtag)"
            case None => withMethodAndSummary
          }
          val withDefnTag = method.tag match {
            case Some(gtag) => q"$withGroupTag.tag($gtag)"
            case None => withGroupTag
          }

          withDefnTag
        }

        def inferOutputTypeEncoder(tpe: Type): Tree = {
          c.inferImplicitValue(appliedType(peoType.typeConstructor, tpe)) match {
            case EmptyTree => mirror.defaultBody match {
              case Some(defBody) => c.inferImplicitValue(appliedType(eocType, tpe, defBodyToTag(defBody))) match {
                case EmptyTree => c.abort(c.enclosingPosition, s"Failed to find PEO or EOC for type $tpe")
                case t => q"$t.instance"
              }
              case None => c.abort(c.enclosingPosition, s"Failed to find PEO and no defaultBody was found")
            }
            case t => q"$t.instance"
          }
        }

        val withErrors = method.output.err match {
          case Some(err) =>
            secInput match {
              case Some(secInput) =>
                q"$endpointWithMethod.errorOut(tan.TapirScalaMacrosInterop.eitherError(${inferOutputTypeEncoder(err.tpe)}, ${secInput.tree}.errorOutput))"
              case None =>
                q"$endpointWithMethod.errorOut(${inferOutputTypeEncoder(err.tpe)})"
            }
          case None =>
            secInput match {
              case Some(secInput) =>
                q"$endpointWithMethod.errorOut(tan.TapirScalaMacrosInterop.onlySecError(${secInput.tree}.errorOutput))"
              case None =>
                endpointWithMethod
            }
        }

        val appErr = method.output.err.map(x => x.tpe).getOrElse(typeOf[Unit])
        val secErr = secInput.map(x => c.typecheck(x.tree.duplicate).tpe.member(TypeName("VErr")).typeSignature).getOrElse(typeOf[Unit])

        val withInputs = if (useNewSecurity) {
          val withPath = q"$withErrors.in($pathInput)"
          val withNonPathInputs = nonPathInputs.foldLeft(withPath)((e, i) => q"$e.in(${i.tree})")

          secInput.foldLeft(withNonPathInputs)((e, i) => q"$e.securityIn(${i.tree}.input).serverSecurityLogic(x => $monadForF.map(${i.tree}.handler(x))(x => x.left.map(x => Right(x): Either[$appErr, $secErr])))")
        } else {
          val withSecInputs = secInput.foldLeft(withErrors)((e, i) => q"$e.in(${i.tree}.input).serverLogicForCurrent(x => $monadForF.map(${i.tree}.handler(x))(x => x.left.map(x => Right(x): Either[$appErr, $secErr])))")
          val withPath = q"$withSecInputs.in($pathInput)"

          nonPathInputs.foldLeft(withPath)((e, i) => q"$e.in(${i.tree})")
        }

        val withOutputs = method.output.out match {
          case Some(out) => q"$withInputs.out(${inferOutputTypeEncoder(out.tpe)})"
          case None => withInputs
        }

        withOutputs
      }

      val hasSec = secInput.nonEmpty
      val multiInput = (pathInputs.size + nonPathInputs.size) > 1

      if (hasSec)
        q"$endpointDef.serverLogic(${tupleMappingTree(hasSec, multiInput)})"
      else
        q"$endpointDef.serverLogic[$fTpe](${tupleMappingTree(hasSec, multiInput)})"
    }

    q"List(..$endpoints)"
  }
}
