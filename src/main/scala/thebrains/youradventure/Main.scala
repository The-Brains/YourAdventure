package thebrains.youradventure

import scalaz.zio.{App, IO}
import thebrains.youradventure.Adventure.ActionPack.ActionCollection
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FPTerminalIO._
import thebrains.youradventure.Utils.Error

object Main extends App {
  override def run(args: List[String]): IO[Nothing, ExitStatus] = {
    val tp = TerminalPrint()
    val game = GameStatus(
      Universe(
        availableRaces = List(
          Races.Human
        ),
        startingStep = Step(
          name = "Starting step",
          description = "bla bla bla",
          location = Locations.Earth,
          transformations = TransformationCollection.Empty,
          availableActions = ActionCollection.Empty
        )
      )
    )

    myAppLogic(tp, game)
      .attempt
      .map(_.fold(e => {
        tp.display(Renderer().display(e))
        1
      }, _ => 0))
      .map(ExitStatus.ExitNow(_))
  }

  def myAppLogic(tp: TerminalPrint, game: GameStatus): IO[Error, GameStatus] = {
    for {
      n <- tp.render(game)
      g <- myAppLogic(tp, n)
    } yield {
      g
    }
  }
}
