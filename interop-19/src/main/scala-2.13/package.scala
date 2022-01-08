import io.circe.{Encoder, Decoder}
import sttp.model.StatusCode
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.{statusCode, EndpointOutput, Schema, EndpointInput}

import scala.language.experimental.macros

package object tan {
  import attrs._

  @tapirVersion("19")
  trait Controller[F[_]]

  def compile[Cls <: Controller[Eff], Eff[_]](cls: Cls): List[ServerEndpoint[Any, Eff]] = macro tan.tmacro.scala2.compileImpl2[Cls, Eff]

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

  trait Security[VOut, F[_]] {
    type VIn
    type VErr

    val input: EndpointInput[VIn]
    val errorOutput: EndpointOutput[VErr]
    def handler(in: VIn): F[Either[VErr, VOut]]
  }
  object Security {
    type Aux[VIn_, VOut, VErr_, F[_]] = Security[VOut, F] { type VIn = VIn_; type VErr = VErr_ }
  }
}