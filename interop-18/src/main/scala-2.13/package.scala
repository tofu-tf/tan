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

  def compile[Cls <: Controller[Eff], Eff[_]](cls: Cls): List[ServerEndpoint[_, _, _, Any, Eff]] = macro tan.tmacro.scala2.compileImpl2[Cls, Eff]
}