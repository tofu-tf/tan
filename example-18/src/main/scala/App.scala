import sttp.model.StatusCode
import sttp.tapir._

import tan._
import tan.attrs._

import scala.language.experimental.macros

object App {
  type IO[X] = X

  case class SecureThingy(wtf: String)
  object SecureThingy {
    implicit val security: Security[String, SecureThingy, IO] = new Security[String, SecureThingy, IO] {
      override val input: EndpointInput[String] = auth.bearer[String]()
      override def handler(in: String): IO[Either[Unit, SecureThingy]] = Right(SecureThingy("kek" + in))
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

    @get("a/{foo}/c/{qux}")
    def x(foo: String, @query bar: String, qux: Int): IO[Either[StatusCode, String]] = Right("")

    @post("a/{foo}/c/{qux}")
    def y(foo: String, @query bar: String, qux: Int, @security secure: SecureThingy, @body ptxt: String): String = ""

    @put("a/{foo}/c/{qux}")
    def z(foo: String, @query bar: String, qux: Int): List[String] = Nil

    @delete("a/{foo}/c/{qux}")
    def zw(foo: String, @query bar: String, qux: Int): Either[StatusCode, Unit] = Left(StatusCode.Ok)
  }

  def main(args: Array[String]): Unit = {
    val endp = endpoint
      .post
      .in(query[String]("foo"))
      .in(path[String]("bar"))
      .out(plainBody[String])

    val wtf = new Wtf
    val endpm1 = compile[Wtf, IO](wtf)

    println(endp.show)
    println(endpm1.map(_.show).mkString(";\n"))
  }
}