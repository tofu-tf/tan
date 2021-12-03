import io.circe.{Encoder, Decoder}
import sttp.model.StatusCode
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.{statusCode, EndpointOutput, Schema, EndpointInput}

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

package object tan extends Attrs {
  trait Controller[F[_]]

  def compile[Cls <: Controller[Eff], Eff[_]](cls: Cls): List[ServerEndpoint[_, _, _, Any, Eff]] = macro tan.tmacro.compileImpl[Cls, Eff]

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
