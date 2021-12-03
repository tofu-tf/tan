package tan
package tmacro

object mirror1 {
  sealed trait HttpMethod1
  object HttpMethod1 {
    case object Get extends HttpMethod1
    case object Post extends HttpMethod1
    case object Put extends HttpMethod1
    case object Delete extends HttpMethod1
  }

  sealed trait AnnotationMirror1
  sealed trait DefaultBodyMirror1
  sealed trait NamingConventionMirror1
  sealed trait NamedMirror1
  sealed trait InputSourceMirror1
  object AnnotationMirror1 {
    // Main annots
    case class Method(name: HttpMethod1, path: String) extends AnnotationMirror1

    // Metadata
    case class Tag(tag: String) extends AnnotationMirror1
    case class Summary(tag: String) extends AnnotationMirror1

    // Inputs/outputs
    case object Query extends AnnotationMirror1 with NamedMirror1 with InputSourceMirror1
    case object PathSegment extends NamedMirror1
    case object Body extends AnnotationMirror1 with InputSourceMirror1
    case object Security extends AnnotationMirror1

    // Serialization formats
    case object Json extends AnnotationMirror1 with DefaultBodyMirror1
    case object PlainText extends AnnotationMirror1 with DefaultBodyMirror1

    // IO misc
    case class DefaultBody(tpe: DefaultBodyMirror1) extends AnnotationMirror1

    // Naming
    case class NamingConvention(target: NamedMirror1, convention: NamingConventionMirror1) extends AnnotationMirror1
    case object Snake extends NamingConventionMirror1
    case object SpaceSnake extends NamingConventionMirror1
    case object Kebab extends NamingConventionMirror1
  }

  case class OutputTypeMirror1[Tpe](
    higher: Boolean,
    either: Boolean,
    out: Option[Tpe],
    err: Option[Tpe]
  ) {
    override def toString: String = {
      if (higher) {
        if (either)
          s"F[Either[error: $err, out: $out]]"
        else
          s"F[out: $out]"
      } else {
        if (either)
          s"Id[Either[error: $err, out: $out]]"
        else
          s"Id[out: $out]"
      }
    }
  }

  case class ParamMirror1[Tpe](
    name: String,
    tpe: Tpe,
    annotations: List[AnnotationMirror1]
  ) {
    override def toString: String = s"@(${annotations.mkString(", ")}) $name: $tpe"
  }

  case class MethodMirror1[Tree, Tpe](
    name: String,
    params: List[ParamMirror1[Tpe]],
    outputType: OutputTypeMirror1[Tpe],
    callTarget: Tree,
    annotations: List[AnnotationMirror1]
  ) {
    override def toString: String = {
      val paramsStr = params.mkString(",\n")

      s"@(${annotations.mkString(", ")}) def `$name': $outputType (\n$paramsStr\n)"
    }
  }

  case class ControllerMirror1[Tree, Tpe](
    name: String,
    annotations: List[AnnotationMirror1],
    methods: List[MethodMirror1[Tree, Tpe]]
  ) {
    override def toString: String = {
      val methodStr = methods.map(_.toString.linesIterator.map(s => "  " + s).mkString("\n")).mkString("\n\n")

      s"@(${annotations.mkString(", ")}) controller `$name' {\n$methodStr\n}"
    }
  }
}
