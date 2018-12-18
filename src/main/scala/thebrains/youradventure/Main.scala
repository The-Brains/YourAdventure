package thebrains.youradventure

import scalaz.ImmutableArray
import scalaz.effect.{IO, SafeApp}
import thebrains.youradventure.terminalUI.{GameStatus, TerminalUI}

object Main extends SafeApp {
  override def run(args: ImmutableArray[String]): IO[Unit] = {
    TerminalUI.run(
      GameStatus(None, None),
      None
    )
    super.run(args)
  }
}
