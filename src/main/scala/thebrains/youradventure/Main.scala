package thebrains.youradventure

import scalaz.ImmutableArray
import scalaz.effect.{IO, SafeApp}
import thebrains.youradventure.Adventure.State

object Main extends SafeApp {
  override def run(args: ImmutableArray[String]): IO[Unit] = {
    super.run(args)
  }
}
