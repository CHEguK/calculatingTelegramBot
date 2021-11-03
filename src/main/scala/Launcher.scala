import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import com.typesafe.config.ConfigFactory


object Launcher extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val token: String = ConfigFactory.load("application.conf").getString("bot.token")
    new EchoBot[IO](token).startPolling().map(_ => ExitCode.Success)
  }
}