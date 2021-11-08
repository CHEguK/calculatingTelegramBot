import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp


object Launcher extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val token: String = System.getenv("TOKEN") //ConfigFactory.load("application.conf").getString("bot.token")  // Если нужны параметры из конфига
    new EchoBot[IO](token).startPolling().map(_ => ExitCode.Success)
  }
}