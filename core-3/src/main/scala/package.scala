import sttp.model.StatusCode
import tan.attrs.DefTags
import sttp.tapir.{Schema, EndpointInput, EndpointOutput}
import io.circe.{Encoder, Decoder}
import sttp.tapir.statusCode
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.server.ServerEndpoint

package object tan {
  import attrs._

  @tapirVersion("18")
  trait Controller[F[_]]

  inline def compile[Cls <: Controller[Eff], Eff[_]](cls: Cls): List[ServerEndpoint[_, _, _, Any, Eff]] = ${ tan.tmacro.scala3.compileImpl3[Cls, Eff, ServerEndpoint[_, _, _, Any, Eff]]('{ cls }) }

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

  trait Security[VIn, VOut, F[_]] {
    val input: EndpointInput[VIn]
    def handler(in: VIn): F[Either[Unit, VOut]]
  }
}
