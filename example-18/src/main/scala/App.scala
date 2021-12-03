import cats.effect.*
import sttp.model.StatusCode
import sttp.tapir.{query as fuk2, json as fuk, *}
import sttp.tapir.server.http4s.*
import sttp.tapir.docs.openapi._
import sttp.tapir.openapi.OpenAPI
import sttp.tapir.openapi.circe.yaml._
import org.http4s.ember.server.*
import org.http4s.*
import tan.*
import tan.attrs.*

import scala.language.experimental.macros

object App extends IOApp {
  case class SecureThingy(wtf: String)
  object SecureThingy {
    implicit val security: Security[String, SecureThingy, IO] = new Security[String, SecureThingy, IO] {
      override val input: EndpointInput[String] = auth.bearer[String]()
      override def handler(in: String): IO[Either[Unit, SecureThingy]] = IO { Right(SecureThingy("kek" + in)) }
    }
  }

  @defaultBody[json]
  @tag("taggy")
  @query
  @json
  @get("a/b/c")
  @namingConvention[query -> spaceSnake]
  @namingConvention[pathSeg -> kebab]
  class Wtf extends Controller[IO] {

    private def wtf(): String = "hi"

    @get("a/{foo}/c/{qux}")
    def x(foo: String, @query bar: String, qux: Int): IO[Either[StatusCode, String]] = IO {
      Right(s"foo=$foo, bar=$bar, qux=$qux")
    }

    @post("a/{foo}/c/{qux}")
    def y(foo: String, @query bar: String, qux: Int, /* @security secure: SecureThingy, */ @body ptxt: String): String = ""

    @put("a/{foo}/c/{qux}")
    def z(foo: String, @query bar: String, qux: Int, @query bark: Boolean): List[String] = Nil

    @delete("a/{foo}/c/{qux}")
    def zw(foo: String, @query bar: String, qux: Int): Either[StatusCode, Unit] = Left(StatusCode.Ok)
  }

  def run(args: List[String]): IO[ExitCode] = {
    val endp = endpoint
      .post
      .in(fuk2[String]("foo"))
      .in(path[String]("bar"))
      .out(plainBody[String])

    val wtf = new Wtf
    val endpm1 = compile[Wtf, IO](wtf)

    println(endp.show)
    println(endpm1.map(_.show).mkString(";\n"))

    val openapi: OpenAPI = OpenAPIDocsInterpreter().serverEndpointsToOpenAPI(endpm1, "endpm1", "0.1")
    println(openapi.toYaml)

    val r = Http4sServerInterpreter[IO]().toRoutes(endpm1)
    val app: HttpApp[IO] = r.orNotFound

    EmberServerBuilder.default[IO]
      .withHttpApp(app)
      .build
      .use { _ => IO.never }
  }
}