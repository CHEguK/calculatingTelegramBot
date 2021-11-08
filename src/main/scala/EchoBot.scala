import cats.effect.{Concurrent, ContextShift}
import cats.syntax.functor._
import com.bot4s.telegram.cats.Polling
import com.bot4s.telegram.methods._
import com.bot4s.telegram.api.declarative.{Commands, RegexCommands}
import com.bot4s.telegram.models._
import parser.{Evaluator, FormulaParser}

import scala.util.matching.Regex


class EchoBot[F[_]: Concurrent: ContextShift](token: String) extends ExampleBot[F](token) with Polling[F] with Commands[F]{

  override def receiveMessage(msg: Message): F[Unit] = {


    val vars = Map(
      "pi" -> math.Pi,
      "e" -> math.E)

    val funcs: Map[String, (Double) => Double] = Map(
      "sin" -> math.sin,
      "cos" -> math.cos,
      "inc" -> { d: Double => d + 1 }
    )

    msg.text.fold(unit) { text =>
      val u = FormulaParser(text)
      u match {
        case Left(error) => request(SendMessage(msg.source, s"\'$text\' parsing error: $error")).void
        case Right(value) => request(SendMessage(msg.source, Evaluator(value, vars, funcs).toString)).void
      }
    }
  }

}