package thebrains.youradventure

import scalaz.zio.{App, IO}
import thebrains.youradventure.Adventure.ActionPack.ActionCollection
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FPTerminalIO._
import thebrains.youradventure.Game.GameStatus
import thebrains.youradventure.Utils.Error

object Main extends App {
  override def run(args: List[String]): IO[Nothing, ExitStatus] = {
    val renderer = Renderer()
    val tp = TerminalPrint()
    val game = GameStatus(
      renderer,
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

    myAppLogic(tp, game).attempt
      .flatMap[Error, Unit] {
        case Left(e) =>
          (for {
            m <- renderer.display(e)
            _ <- tp.display(m)
            m <- renderer.displayEmptyLine
            _ <- tp.display(m)
          } yield {
            ()
          }).flatMap { _ =>
            IO.fail(e)
          }
        case Right(g) =>
          for {
            m <- renderer.display(g.toString)
            _ <- tp.display(m, ignoreLineLength = true)
          } yield {}
      }
      .attempt
      .map[Int] {
        case Left(_: Error) => 1
        case Right(_) => 0
      }
      .map[ExitStatus](ExitStatus.ExitNow(_))
  }

  def myAppLogic(
    tp:   TerminalPrint,
    game: GameStatus
  ): IO[Error, GameStatus] = {
    for {
      n <- tp.render(game)
      g <- if (game.isCompleted) IO.sync(game) else myAppLogic(tp, n)
    } yield {
      g
    }
  }
}
