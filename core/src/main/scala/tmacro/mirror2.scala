package tan
package tmacro

import tmacro.mirror1._

object mirror2 {
  sealed trait NamingConvention2
  object NamingConvention2 {
    case object AsIs extends NamingConvention2
    case object Snake extends NamingConvention2
    case object SpaceSnake extends NamingConvention2
    case object Kebab extends NamingConvention2
  }

  case class NamingConventions2(
    query: NamingConvention2,
    pathSeg: NamingConvention2
  )

  sealed trait MethodInput2[Tpe] {
    val tpe: Tpe
  }
  object MethodInput2 {
    case class Path[Tpe](name: String, tpe: Tpe) extends MethodInput2[Tpe]
    case class Query[Tpe](name: String, tpe: Tpe) extends MethodInput2[Tpe]
    case class Body[Tpe](bodyType: Option[DefaultBodyMirror1], tpe: Tpe) extends MethodInput2[Tpe]
    case class Security[Tpe](tpe: Tpe) extends MethodInput2[Tpe]
  }

  case class MethodOutputSide2[Tpe](bodyType: Option[DefaultBodyMirror1], tpe: Tpe)
  case class MethodOutput2[Tpe](
    higher: Boolean,
    either: Boolean,
    err: Option[MethodOutputSide2[Tpe]],
    out: Option[MethodOutputSide2[Tpe]],
  )

  sealed trait PathSegment2
  object PathSegment2 {
    case class Raw(value: String) extends PathSegment2
    case class Subst(name: String) extends PathSegment2
  }

  case class MethodMirror2[Tree, Tpe](
    name: String,
    method: HttpMethod1,
    pathSegments: List[PathSegment2],
    inputs: List[MethodInput2[Tpe]],
    output: MethodOutput2[Tpe],
    callTarget: Tree,
    tag: Option[String],
    summary: Option[String],
  )

  case class ControllerMirror2[Tree, Tpe](
    name: String,
    defaultBody: Option[DefaultBodyMirror1],
    namingConventions: NamingConventions2,
    tag: Option[String],
    methods: List[MethodMirror2[Tree, Tpe]]
  )
}
