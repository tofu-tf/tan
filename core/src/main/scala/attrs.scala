package tan

import scala.annotation.StaticAnnotation

object attrs {
  final class tapirVersion(version: String) extends StaticAnnotation

  object DefTags {
    trait JsonDefTag
    trait PlainTextDefTag
  }

  sealed trait DefaultBody
  sealed trait Named
  sealed trait NamingConvention

  final class tag(str: String) extends StaticAnnotation
  final class summary(str: String) extends StaticAnnotation

  final class get(str: String) extends StaticAnnotation
  final class post(str: String) extends StaticAnnotation
  final class put(str: String) extends StaticAnnotation
  final class delete(str: String) extends StaticAnnotation

  final class query extends StaticAnnotation with Named
  final class body extends StaticAnnotation
  final class security extends StaticAnnotation

  final class defaultBody[d <: DefaultBody] extends StaticAnnotation
  final class namingConvention[conv <: ->[_, _]] extends StaticAnnotation

  final class json extends StaticAnnotation with DefaultBody
  final class plainText extends StaticAnnotation with DefaultBody

  final class ->[named <: Named, conv <: NamingConvention]

  final class snake extends NamingConvention
  final class spaceSnake extends NamingConvention
  final class kebab extends NamingConvention

  final class pathSeg extends Named
}
