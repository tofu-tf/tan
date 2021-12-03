package tan
package tmacro

import tmacro.mirror1.{ControllerMirror1, AnnotationMirror1, DefaultBodyMirror1}
import tmacro.mirror2.{ControllerMirror2, PathSegment2, MethodOutputSide2, NamingConvention2, MethodOutput2, NamingConventions2, MethodMirror2, MethodInput2}

object postprocessor {
  def apply[Tree, Tpe](mirror1: ControllerMirror1[Tree, Tpe]): ControllerMirror2[Tree, Tpe] = {
    ControllerMirror2(
      name = mirror1.name,
      defaultBody = mirror1.annotations.collectFirst {
        case AnnotationMirror1.DefaultBody(db) => db
      },
      namingConventions = {
        val plain = mirror1.annotations.collect {
          case AnnotationMirror1.NamingConvention(tgt, cnv) =>
            val newCnv = cnv match {
              case AnnotationMirror1.Snake => NamingConvention2.Snake
              case AnnotationMirror1.SpaceSnake => NamingConvention2.SpaceSnake
              case AnnotationMirror1.Kebab => NamingConvention2.Kebab
            }

            tgt -> newCnv
        }.toMap

        NamingConventions2(
          query = plain.getOrElse(AnnotationMirror1.Query, NamingConvention2.AsIs),
          pathSeg = plain.getOrElse(AnnotationMirror1.PathSegment, NamingConvention2.AsIs)
        )
      },
      tag = mirror1.annotations.collectFirst {
        case AnnotationMirror1.Tag(tag) => tag
      },
      methods = mirror1.methods.map { meth =>
        val methodAnnot = meth.annotations.collectFirst {
          case m: AnnotationMirror1.Method => m
        }.getOrElse(throw new Exception("No method annot")) // TODO: use compiler-provided aborts

        val segmentedPath = methodAnnot.path.split('/').map {
          case s"{$name}" => PathSegment2.Subst(name)
          case s => PathSegment2.Raw(s)
        }.toList

        val inputs: List[MethodInput2[Tpe]] = meth.params.map { param =>
          val inputType = param.annotations.collectFirst {
            case AnnotationMirror1.Query => MethodInput2.Query(param.name, param.tpe)
            case AnnotationMirror1.Body =>
              val defBody = param.annotations.collectFirst[DefaultBodyMirror1] {
                case AnnotationMirror1.Json => AnnotationMirror1.Json
                case AnnotationMirror1.PlainText => AnnotationMirror1.PlainText
              }

              MethodInput2.Body(defBody, param.tpe)
            case AnnotationMirror1.Security =>
              MethodInput2.Security(param.tpe)
          }

          inputType.getOrElse(MethodInput2.Path(param.name, param.tpe))
        }

        val output = MethodOutput2(
          higher = meth.outputType.higher,
          either = meth.outputType.either,
          out = meth.outputType.out.map(o => MethodOutputSide2(None, o)),
          err = meth.outputType.err.map(e => MethodOutputSide2(None, e)),
        )

        MethodMirror2(
          name = meth.name,
          method = methodAnnot.name,
          pathSegments = segmentedPath,
          inputs = inputs,
          output = output,
          callTarget = meth.callTarget,
          tag = meth.annotations.collectFirst {
            case AnnotationMirror1.Tag(tag) => tag
          },
          summary = meth.annotations.collectFirst {
            case AnnotationMirror1.Summary(tag) => tag
          },
        )
      }
    )
  }
}
