package thebrains.youradventure

import scalaz.ImmutableArray
import scalaz.effect.{IO, SafeApp}
import thebrains.youradventure.Adventure.Universe
import thebrains.youradventure.TerminalUIPack.{GameStatus, TerminalUI}

object Main extends SafeApp {
  override def run(args: ImmutableArray[String]): IO[Unit] = {
    TerminalUI().run(
      GameStatus(Universe.Void),
      None
    )
    super.run(args)
  }
}
