import cats.effect._
import sttp.model.StatusCode
import sttp.tapir.{query => fuk2, json => fuk, _}
import sttp.tapir.server.http4s._
import sttp.tapir.docs.openapi._
import sttp.tapir.openapi.OpenAPI
import sttp.tapir.openapi.circe.yaml._
import org.http4s.ember.server._
import org.http4s._
import tan._
import tan.attrs._

import scala.language.experimental.macros
import scala.concurrent.duration._

object App extends IOApp {
  case class SecureThingy(wtf: String)
  object SecureThingy {
    implicit val security: Security.Aux[String, SecureThingy, Unit, IO] = new Security[SecureThingy, IO] {
      type VIn = String
      type VErr = Unit

      override val input: EndpointInput[String] = sttp.tapir.query[String]("wowsuchsecure")//auth.bearer[String]()
      override val errorOutput: EndpointOutput[Unit] = sttp.tapir.emptyOutputAs(())
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
    def x(foo: String, @query bar: String, qux: Int, @security secure: SecureThingy): IO[Either[StatusCode, String]] = IO {
      Right(s"foo=$foo, bar=$bar, qux=$qux, security=$secure")
    }

    @post("a/{foo}/c/{qux}")
    def y(foo: String, @query bar: String, qux: Int, @security secure: SecureThingy, @body ptxt: String, @query kekekek: String): String = ""

    @put("a/{foo}/c/{qux}")
    def z(foo: String, @query bar: String, qux: Int, @query bark: Boolean): List[String] = Nil

    @delete("a/{foo}/c/{qux}")
    def zw(foo: String, @query bar: String, qux: Int): Either[StatusCode, Unit] = Left(StatusCode.Ok)

    @delete("a")
    def zw0(): Unit = ()

    @delete("a")
    def zw1(@query single: Int): IO[Unit] = IO.unit
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
      .use { _ => IO.sleep(5.seconds).as(ExitCode.Success) }
  }
}