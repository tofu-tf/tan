package tan

import cats.Monad
import cats.syntax.either._
import sttp.tapir._
import sttp.tapir.CodecFormat.TextPlain
import sttp.tapir.EndpointOutput.StatusCode
import sttp.tapir.typelevel.ParamConcat

import tmacro.mirror1._

object TapirScalaMacrosInterop {
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

  def eitherError[A, B](logic: EndpointOutput[A], security: EndpointOutput[B]): EndpointOutput[Either[A, B]] =
    oneOf(
      oneOfVariantValueMatcher(sttp.model.StatusCode.InternalServerError, logic.map(Left(_))(_.value)) { case Left(_) => true },
      oneOfVariantValueMatcher(sttp.model.StatusCode.Unauthorized, security.map(Right(_))(_.value)) { case Right(_) => true }
    )

  def onlySecError[B](security: EndpointOutput[B]): EndpointOutput[Either[Unit, B]] =
    oneOf(
      oneOfVariantValueMatcher(sttp.model.StatusCode.InternalServerError, emptyOutput.map(Left(_))(_.value)) { case Left(_) => true },
      oneOfVariantValueMatcher(sttp.model.StatusCode.Unauthorized, security.map(Right(_))(_.value)) { case Right(_) => true }
    )

  def query[T](name: String)(implicit c: Codec[List[String], T, TextPlain]): EndpointInput.Query[T] =
    sttp.tapir.query[T](name)

  def path[T](name: String)(implicit c: Codec[String, T, TextPlain]): EndpointInput.PathCapture[T] =
    sttp.tapir.path[T](name)

  def plainBody[T](implicit c: Codec[String, T, TextPlain]): EndpointIO.Body[String, T] =
    sttp.tapir.plainBody[T]

  def stringToPath(s: String): EndpointInput.FixedPath[Unit] =
    sttp.tapir.stringToPath(s)

  def right[E, A](a: A): Either[E, A] = Right(a)

  def coerceResultToSecure[F[_], LE, SE, R](monad: Monad[F], a: F[Either[LE, R]]): F[Either[Either[LE, SE], R]] =
    monad.map(a)(_.leftMap(_.asLeft))
  
  def leftWrap[E, E2, A](e: Either[E, A]): Either[Either[E2, E], A] = e.left.map(x => Right(x))
}
