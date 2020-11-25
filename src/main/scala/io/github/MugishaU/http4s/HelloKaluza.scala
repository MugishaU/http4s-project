package io.github.MugishaU.http4s

import cats.Applicative
import cats.implicits._
import io.circe.{Encoder, Json}
import org.http4s.EntityEncoder
import org.http4s.circe._
import java.time._

trait HelloKaluza[F[_]]{
  def hello(n: HelloKaluza.Name): F[HelloKaluza.Greeting]
}

object HelloKaluza {
  implicit def apply[F[_]](implicit ev: HelloKaluza[F]): HelloKaluza[F] = ev

  final case class Name(name: String) extends AnyVal
  /**
    * More generally you will want to decouple your edge representations from
    * your internal data structures, however this shows how you can
    * create encoders for your data.
    **/
  final case class Greeting(greeting: String) extends AnyVal
  object Greeting {
    implicit val greetingEncoder: Encoder[Greeting] = new Encoder[Greeting] {
      final def apply(a: Greeting): Json = Json.obj(
        ("message", Json.fromString(a.greeting)),
        ("access-date",Json.fromString(LocalDate.now().toString)),
        ("access-time",Json.fromString(LocalTime.now().toString))
      )
    }
    implicit def greetingEntityEncoder[F[_]: Applicative]: EntityEncoder[F, Greeting] =
      jsonEncoderOf[F, Greeting]
  }

  def impl[F[_]: Applicative]: HelloKaluza[F] = new HelloKaluza[F]{
    def hello(n: HelloKaluza.Name): F[HelloKaluza.Greeting] =
      Greeting("Welcome to Kaluza " + n.name + "! This is the future of energy ⚡.").pure[F]
  }
}