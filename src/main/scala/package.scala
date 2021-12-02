import io.circe.{Encoder, Decoder}
import sttp.model.StatusCode
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.{statusCode, EndpointOutput, Schema, EndpointInput}

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

package object tan {
  trait Controller[F[_]]

  def compile[Cls <: Controller[Eff], Eff[_]](cls: Cls): List[ServerEndpoint[_, _, _, Any, Eff]] = macro tan.tmacro.compileImpl[Cls, Eff]

  // markup attributes and stuff

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

  // wrappers for EndpointIO

  case class ProvidedEndpointInput[T](instance: EndpointInput[T]) extends AnyVal
  case class ProvidedEndpointOutput[T](instance: EndpointOutput[T]) extends AnyVal
  object ProvidedEndpointOutput {
    implicit val psc: ProvidedEndpointOutput[StatusCode] = ProvidedEndpointOutput(statusCode)
  }

  trait EndpointInputConstructor[V, DefTag] {
    def instance: EndpointInput[V]
  }
  object EndpointInputConstructor {
    implicit def constructorForJsonTag[V: Decoder: Encoder: Schema]: EndpointInputConstructor[V, DefTags.JsonDefTag] = new EndpointInputConstructor[V, DefTags.JsonDefTag] {
      override val instance: EndpointInput[V] = jsonBody[V]
    }
  }

  trait EndpointOutputConstructor[V, DefTag] {
    def instance: EndpointOutput[V]
  }
  object EndpointOutputConstructor {
    implicit def constructorForJsonTag[V: Decoder: Encoder: Schema]: EndpointOutputConstructor[V, DefTags.JsonDefTag] = new EndpointOutputConstructor[V, DefTags.JsonDefTag] {
      override val instance: EndpointOutput[V] = jsonBody[V]
    }
  }

  object DefTags {
    trait JsonDefTag
  }

  trait Security[VIn, VOut, F[_]] {
    val input: EndpointInput[VIn]
    def handler(in: VIn): F[Either[Unit, VOut]]
  }
}
