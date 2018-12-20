package thebrains.youradventure.Adventure

import scalaz.Maybe
import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.Adventure.AttributePack.Attributes
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.ParentTest
import thebrains.youradventure.TerminalUIPack._
import thebrains.youradventure.Utils.BirdUtils.BirdOperator._
import thebrains.youradventure.Utils.Error

class StepTest extends ParentTest {
  "Step" - {
    lazy val step = Step(
      name = "starting Step",
      description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. " +
        "Nunc hendrerit vehicula pellentesque. Mauris vitae pellentesque risus, ut " +
        "lacinia diam. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In dui " +
        "magna, blandit non justo ornare, pellentesque dictum purus. Phasellus eu lacinia " +
        "elit. Quisque nec turpis non massa mattis blandit. Duis blandit quis turpis quis " +
        "luctus. Suspendisse scelerisque leo dui, a dictum lacus semper elementum. " +
        "Praesent pulvinar tristique libero eu iaculis. Phasellus cursus vel dui ut laoreet. " +
        "Sed faucibus in nibh ac iaculis. Nam dui metus, venenatis vitae diam ac, ornare " +
        "tempor nisl. Pellentesque ornare est at augue pellentesque volutpat. Praesent " +
        "feugiat orci in est iaculis, id pellentesque elit pretium. Proin sit amet nibh eget " +
        "sem convallis pellentesque non nec tortor. Nulla a imperdiet augue, vel fermentum ipsum.",
      location = Locations.Earth,
      transformations = TransformationCollection(
        TransformationBuilder
          .willDo(Addition)
          .byValueOf(10)
          .onAttribute(Attributes.Strength)
      ),
      availableActions = ActionCollection("Whats up?")(
        Action("Look", "Look at Earth closer", Steps.EmptyStep),
        Action("Leave", "Leave earth alone", Steps.EmptyStep)
      )
    )

    val r: Renderer = Renderer(TerminalPrint())

    def processStep(
      r:      Renderer,
      player: Maybe[Player],
      step:   Step,
      answer: Maybe[String]
    ): Maybe[Either[Error, Maybe[Action]]] = {
      r.display(step, player)
        .map {
          case (tp, message) =>
            message match {
              case Left(m: DisplayQuestion) =>
                m.displayWithQuestionAnd(tp, answer) |>
                  step.getActions(player).getAction
              case Right(m: DisplayMessage) =>
                m displayWith tp
                Right(Maybe.empty)
            }
        }
    }

    def processGame(
      r:       Renderer,
      player:  Maybe[Player],
      step:    Step,
      answers: List[Maybe[String]]
    ): Unit = {
      processStep(r, player, step, answers.headOption match {
        case Some(a) => a
        case None    => Maybe.empty
      }).map {
        case Left(error) =>
          r.display(error)
            .map {
              case (tp, message) =>
                message displayWith tp
            }
          if (!error.isFatal) {
            processGame(r, player, step, answers.drop(1))
          }
        case Right(Maybe.Just(action)) =>
          processGame(r, player, action.targetStep, answers.drop(1))
        case Right(Maybe.Empty()) => () // End of game
      }
    }

    "Display" in {
      processGame(
        r,
        Maybe.empty,
        step,
        List(
          Maybe.Just("1")
        )
      )
    }

    "Display with player" in {
      processGame(
        r,
        Maybe.Just(
          PlayerBuilder.create("test").selectRace(Races.Human)
        ),
        step,
        List(
          Maybe.Just("0"),
          Maybe.Just("0"),
          Maybe.Just("0"),
          Maybe.Just("1"),
          Maybe.Just("2")
        )
      )
    }
  }
}
