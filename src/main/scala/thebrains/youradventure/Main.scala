package thebrains.youradventure

import scalaz.zio.{App, IO}
import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.Adventure.AttributePack.Attributes
import thebrains.youradventure.Adventure.StepPack._
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FPTerminalIO._
import thebrains.youradventure.Game.GameStatus
import thebrains.youradventure.Utils.Error

object Main extends App {
  override def run(args: List[String]): IO[Nothing, ExitStatus] = {
    val renderer = Renderer()
    val tp = TerminalPrint()
    (for {
      universe <- Universe(
        availableRaces = List(
          Races.Human
        ),
        availableSteps = StepCollection.Empty,
        startingStep = Step(
          name = "Your birth",
          description = "You open your eye and see the darkness of the cave your" +
            " civilization is evolving.",
          location = Locations.Earth,
          transformations = TransformationCollection.Empty,
          availableActions = ActionCollection("What are you doing?")(
            Action(
              "Scream",
              "You are screaming to open up your lungs",
              Right(
                Step(
                  name = "Alive",
                  description = "You are alive",
                  location = Locations.Earth,
                  transformations = TransformationCollection(
                    TransformationBuilder
                      .willDo(Addition)
                      .byValueOf(1)
                      .onAttribute(Attributes.Strength)
                  ),
                  availableActions = ActionCollection.Empty
                )
              )
            ),
            Action(
              "Die",
              "You are one of the numerous baby which don't make it",
              Right(Steps.EmptyStep)
            )
          )
        )
      )
      game = GameStatus(
        renderer,
        universe
      )
      exit <- myAppLogic(tp, game).attempt
    } yield {
      exit
    }).flatMap[Error, Unit] {
        case Left(e) =>
          (for {
            m <- renderer.displayErrorAsMessage(e)
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
