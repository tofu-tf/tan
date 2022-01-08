import io.circe.{Encoder, Decoder}
import sttp.model.StatusCode
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.{statusCode, EndpointOutput, Schema, EndpointInput}

import scala.language.experimental.macros

package object tan {
  import attrs._

  @tapirVersion("18")
  trait Controller[F[_]]

  inline def compile[Cls <: Controller[Eff], Eff[_]](cls: Cls): List[ServerEndpoint[_, _, _, Any, Eff]] = ${ tan.tmacro.scala3.compileImpl3[Cls, Eff, ServerEndpoint[_, _, _, Any, Eff]]('{ cls }) }
}