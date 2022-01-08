package tan
package tmacro
package scala3

import mirror1.*
import attrs.*

import scala.quoted.*

object reflector {
  def apply[Cls: Type, F[_]: Type](cls: Expr[Cls])(using q: Quotes): ControllerMirror1[q.reflect.Symbol, q.reflect.TypeRepr] = {
    import q.reflect.*
    import q.reflect.report.*

    val _tag = TypeRepr.of[tag].typeSymbol
    val _summary = TypeRepr.of[summary].typeSymbol

    val _get = TypeRepr.of[get].typeSymbol
    val _post = TypeRepr.of[post].typeSymbol
    val _put = TypeRepr.of[put].typeSymbol
    val _delete = TypeRepr.of[delete].typeSymbol

    val _query = TypeRepr.of[query].typeSymbol
    val _pathSeg = TypeRepr.of[pathSeg].typeSymbol
    val _body = TypeRepr.of[body].typeSymbol
    val _security = TypeRepr.of[security].typeSymbol

    val _defaultBody = TypeRepr.of[defaultBody[_]].typeSymbol
    val _namingConvention = TypeRepr.of[namingConvention[_]].typeSymbol

    val _json = TypeRepr.of[json].typeSymbol
    val _plainText = TypeRepr.of[plainText].typeSymbol

    val _snake = TypeRepr.of[snake].typeSymbol
    val _spaceSnake = TypeRepr.of[spaceSnake].typeSymbol
    val _kebab = TypeRepr.of[kebab].typeSymbol

    val fRepr: TypeRepr = TypeRepr.of[F]
    val fSym: Symbol = fRepr.typeSymbol

    val clsRepr: TypeRepr = TypeRepr.of[Cls]
    val clsSym: Symbol = clsRepr.typeSymbol

    val unitSym: Symbol = TypeRepr.of[Unit].typeSymbol
    val eitherSym: Symbol = TypeRepr.of[Either].typeSymbol

    def extractLiteral(tree: Tree): String = tree match {
      case Literal(StringConstant(str)) => str
      case _ => errorAndAbort("Annotation argument is not a literal", tree.pos)
    }

    def extractTypeArg(tpe: TypeRepr, num: Int): TypeRepr = tpe match {
      case AppliedType(_, args) =>
        if (args.size > num)
          args(num)
        else
          errorAndAbort("could not obtain type arg")
    }

    def collectAnnots(symbol: Symbol): List[AnnotationMirror1] =
      symbol.annotations.flatMap { annot =>
        annot.tpe.typeSymbol match {
          case `_query` => Some(AnnotationMirror1.Query)
          case `_body` => Some(AnnotationMirror1.Body)
          case `_security` => Some(AnnotationMirror1.Security)
          case `_json` => Some(AnnotationMirror1.Json)
          case `_plainText` => Some(AnnotationMirror1.PlainText)
          case `_defaultBody` =>
            val defBodyType = extractTypeArg(annot.tpe, 0).typeSymbol match {
              case `_json` => AnnotationMirror1.Json
              case `_plainText` => AnnotationMirror1.PlainText
              case _ => errorAndAbort(s"Unknown defBody type ${annot.tpe}", annot.pos)
            }

            Some(AnnotationMirror1.DefaultBody(defBodyType))
          case `_namingConvention` =>
            val arrow = extractTypeArg(annot.tpe, 0)
            val target = extractTypeArg(arrow, 0).typeSymbol match {
              case `_query` => AnnotationMirror1.Query
              case `_pathSeg` => AnnotationMirror1.PathSegment
              case tgt => errorAndAbort(s"Invalid NC target $tgt", annot.pos)
            }
            val convention = extractTypeArg(arrow, 1).typeSymbol match {
              case `_snake` => AnnotationMirror1.Snake
              case `_spaceSnake` => AnnotationMirror1.SpaceSnake
              case `_kebab` => AnnotationMirror1.Kebab
              case cnv => errorAndAbort(s"Invalid NC convention $cnv", annot.pos)
            }

            Some(AnnotationMirror1.NamingConvention(target, convention))
          case _ => annot match {
            case Apply(_, List(arg0)) =>
              annot.tpe.typeSymbol match {
                case `_get` => Some(AnnotationMirror1.Method(HttpMethod1.Get, extractLiteral(arg0)))
                case `_post` => Some(AnnotationMirror1.Method(HttpMethod1.Post, extractLiteral(arg0)))
                case `_put` => Some(AnnotationMirror1.Method(HttpMethod1.Put, extractLiteral(arg0)))
                case `_delete` => Some(AnnotationMirror1.Method(HttpMethod1.Delete, extractLiteral(arg0)))
                case `_tag` => Some(AnnotationMirror1.Tag(extractLiteral(arg0)))
                case `_summary` => Some(AnnotationMirror1.Summary(extractLiteral(arg0)))
                case _ => None
              }
            case a => println(a); None
          }
        }
      }

    val methods = clsSym.declaredMethods.collect {
      case m if m.isDefDef && !m.isClassConstructor && !m.flags.is(Flags.Private) => m // && check public somehow
    }.map { method =>
      val annots = collectAnnots(method)
      val signature = clsRepr.memberType(method)
      val MethodType(_, _, returnType) = signature

      val outputType = {
        def parseExactOutputType(tpe: TypeRepr): Option[TypeRepr] = {
          val dealiased = tpe.dealias

          if (dealiased.typeSymbol == unitSym)
            None
          else
            Some(tpe)
        }

        def parseOutputType(tpe: TypeRepr, higher: Boolean): OutputTypeMirror1[TypeRepr] = tpe match {
          case AppliedType(ei, List(left, right)) if ei.typeSymbol == eitherSym =>
            OutputTypeMirror1(higher = higher, either = true, out = parseExactOutputType(right), err = parseExactOutputType(left))
          case _ =>
            OutputTypeMirror1(higher = higher, either = false, out = parseExactOutputType(tpe), err = None)
        }

        returnType match {
          case AppliedType(hk, tparams) if hk.typeSymbol == fSym =>
            if (tparams.size == 1)
              parseOutputType(tparams.head, higher = true)
            else
              errorAndAbort("Multi-arg effect type constructors are not supported now")
          case t =>
            parseOutputType(t, higher = false)
        }
      }

      val params = method.paramSymss.head.map { param =>
        val paramTpe = signature.memberType(param)

        ParamMirror1(
          name = param.name,
          tpe = paramTpe,
          annotations = collectAnnots(param)
        )
      }

      MethodMirror1[Symbol, TypeRepr](
        name = method.name,
        params = params,
        outputType = outputType,
        annotations = annots,
        callTarget = method
      )
    }

    ControllerMirror1(
      name = clsSym.name,
      annotations = collectAnnots(clsSym),
      methods = methods
    )
  }
}
