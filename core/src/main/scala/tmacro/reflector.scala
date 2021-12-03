package tan
package tmacro

import scala.reflect.macros.blackbox
import attrs._
import mirror1._

object reflector {
  def apply[Cls: c.WeakTypeTag, F[_]](c: blackbox.Context)(cls: c.Expr[Cls])(implicit _f: c.WeakTypeTag[F[_]]): ControllerMirror1[c.universe.Tree, c.universe.Type] = {
    import c.universe._

    val _tag = symbolOf[tag]
    val _summary = symbolOf[summary]

    val _get = symbolOf[get]
    val _post = symbolOf[post]
    val _put = symbolOf[put]
    val _delete = symbolOf[delete]

    val _query = symbolOf[query]
    val _pathSeg = symbolOf[pathSeg]
    val _body = symbolOf[body]
    val _security = symbolOf[security]

    val _defaultBody = symbolOf[defaultBody[_]]
    val _namingConvention = symbolOf[namingConvention[_]]

    val _json = symbolOf[json]
    val _plainText = symbolOf[plainText]

    val _snake = symbolOf[snake]
    val _spaceSnake = symbolOf[spaceSnake]
    val _kebab = symbolOf[kebab]

    def extractLiteral(tree: Tree): String =
      tree match {
        case Literal(Constant(str: String)) => str
        case _ => c.abort(tree.pos, "Annotation argument is not a literal")
      }

    def extractTypeArg(tpe: Type, num: Int): Type = {
      if (tpe == null)
        c.abort(c.enclosingPosition, "tpe is null")
      else if (tpe.typeArgs.size > num && num >= 0)
        tpe.typeArgs(num)
      else
        c.abort(c.enclosingPosition, "could not obtain type arg")
    }

    def collectAnnots(symbol: Symbol): List[AnnotationMirror1] =
      symbol.annotations.map(_.tree).flatMap {
        case q"new $attr" =>
          attr.symbol match {
            case `_query` => Some(AnnotationMirror1.Query)
            case `_body` => Some(AnnotationMirror1.Body)
            case `_security` => Some(AnnotationMirror1.Security)
            case `_json` => Some(AnnotationMirror1.Json)
            case `_plainText` => Some(AnnotationMirror1.PlainText)
            case `_defaultBody` =>
              val defBodyType = extractTypeArg(attr.tpe, 0).typeSymbol match {
                case `_json` => AnnotationMirror1.Json
                case `_plainText` => AnnotationMirror1.PlainText
                case _ => c.abort(attr.pos, s"Unknown defBody type ${attr.tpe}")
              }

              Some(AnnotationMirror1.DefaultBody(defBodyType))
            case `_namingConvention` =>
              val arrow = extractTypeArg(attr.tpe, 0)
              val target = extractTypeArg(arrow, 0).typeSymbol match {
                case `_query` => AnnotationMirror1.Query
                case `_pathSeg` => AnnotationMirror1.PathSegment
                case tgt => c.abort(attr.pos, s"Invalid NC target $tgt")
              }
              val convention = extractTypeArg(arrow, 1).typeSymbol match {
                case `_snake` => AnnotationMirror1.Snake
                case `_spaceSnake` => AnnotationMirror1.SpaceSnake
                case `_kebab` => AnnotationMirror1.Kebab
                case cnv => c.abort(attr.pos, s"Invalid NC convention $cnv")
              }

              Some(AnnotationMirror1.NamingConvention(target, convention))
            case _ => attr match {
              case q"$call(..$args)" =>
                call.symbol match {
                  case `_get` => Some(AnnotationMirror1.Method(HttpMethod1.Get, extractLiteral(args.head)))
                  case `_post` => Some(AnnotationMirror1.Method(HttpMethod1.Post, extractLiteral(args.head)))
                  case `_put` => Some(AnnotationMirror1.Method(HttpMethod1.Put, extractLiteral(args.head)))
                  case `_delete` => Some(AnnotationMirror1.Method(HttpMethod1.Delete, extractLiteral(args.head)))
                  case `_tag` => Some(AnnotationMirror1.Tag(extractLiteral(args.head)))
                  case `_summary` => Some(AnnotationMirror1.Summary(extractLiteral(args.head)))
                  case _ => None
                }
              case _ => None
            }
          }
        case _ => None
      }

    val fTpe = weakTypeOf[F[_]].typeConstructor
    val clsTpe = weakTypeOf[Cls]
    val clsSym = clsTpe.typeSymbol

    val methods = clsTpe.decls.collect {
      case d if d.isMethod && d.isPublic && !d.isConstructor && d.asMethod.paramLists.size == 1 => d.asMethod
    }.map { method =>
      val annots = collectAnnots(method)

      val outputType = {
        def parseOutputType(tpe: Type, higher: Boolean): OutputTypeMirror1[Type] = {
          def parseExactOutputType(tpe: Type): Option[Type] = {
            val dealiased = tpe.dealias

            if (dealiased.typeSymbol == symbolOf[Unit])
              None
            else
              Some(dealiased)
          }

          if (tpe.typeConstructor.typeSymbol == weakTypeOf[Either[_, _]].typeConstructor.typeSymbol) {
            val List(left, right) = tpe.typeArgs

            OutputTypeMirror1(higher = higher, either = true, out = parseExactOutputType(right), err = parseExactOutputType(left))
          } else {
            OutputTypeMirror1(higher = higher, either = false, out = parseExactOutputType(tpe), err = None)
          }
        }

        method.returnType.typeConstructor.dealias match {
          case tc if tc.takesTypeArgs && tc.typeSymbol == fTpe.typeSymbol =>
            if (tc.typeParams.size == 1)
              parseOutputType(method.returnType.typeArgs.head, higher = true)
            else
              c.abort(c.enclosingPosition, "Multi-arg effect type constructors are not supported now")
          case _ =>
            parseOutputType(method.returnType, higher = false)
        }
      }

      val params = method.paramLists.head.map { param =>
        ParamMirror1(
          name = param.name.decodedName.toString,
          tpe = param.typeSignature,
          annotations = collectAnnots(param)
        )
      }

      MethodMirror1[Tree, Type](
        name = method.name.decodedName.toString,
        params = params,
        outputType = outputType,
        annotations = annots,
        callTarget = q"$cls.${method.name}"
      )
    }.toList

    ControllerMirror1(
      name = clsSym.name.decodedName.toString,
      annotations = collectAnnots(clsSym),
      methods = methods
    )
  }
}
