package tan

import sttp.tapir.*
import sttp.tapir.CodecFormat.TextPlain
import sttp.tapir.typelevel.ParamConcat
import tan.tmacro.mirror1.HttpMethod1

object TapirScala3Interop {
  class FakeParamConcat[L, R, LR](override val leftArity: Int, override val rightArity: Int) extends ParamConcat[L, R] {
    override type Out = LR
  }

  def makeEndpoint(method: HttpMethod1, tags: List[String], summary: String): Endpoint[Unit, Unit, Unit, Unit, Any] = {
    val withMethod = method match {
      case HttpMethod1.Get => endpoint.get
      case HttpMethod1.Post => endpoint.post
      case HttpMethod1.Put => endpoint.put
      case HttpMethod1.Delete => endpoint.delete
    }

    tags.foldLeft(withMethod)(_.tag(_)).summary(summary)
  }

  def query[T: [T] =>> Codec[List[String], T, TextPlain]](name: String): EndpointInput.Query[T] =
    sttp.tapir.query[T](name)

  def path[T: [T] =>> Codec[String, T, TextPlain]](name: String): EndpointInput.PathCapture[T] =
    sttp.tapir.path[T](name)

  def plainBody[T: [T] =>> Codec[String, T, TextPlain]]: EndpointIO.Body[String, T] =
    sttp.tapir.plainBody[T]

  def stringToPath(s: String): EndpointInput.FixedPath[Unit] =
    sttp.tapir.stringToPath(s)

  def right[E, A](a: A): Either[E, A] = Right(a)
  
  def rightWrap[E, E2, A](e: Either[E2, A]): Either[Either[E2, E], A] = e.left.map(x => Left(x))
  def leftWrap[E, E2, A](e: Either[E, A]): Either[Either[E2, E], A] = e.left.map(x => Right(x))
}
