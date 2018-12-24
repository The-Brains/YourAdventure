package thebrains.youradventure.TerminalUIPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.ActionPack._
import thebrains.youradventure.Adventure.AttributePack._
import thebrains.youradventure.Adventure.StepPack.{Step, Steps}
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.Adventure._
import thebrains.youradventure.ParentTest

class GameStatusTest extends ParentTest {
  "GameStatus" - {
    val r: Renderer = Renderer(TerminalPrint())
    val baseGame = GameStatus(
      universe = Universe(
        availableRaces = List(
          Races.Human
        ),
        availableLocations = List(
          Locations.Earth
        ),
        startingStep = Step(
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
            "sem convallis pellentesque non nec tortor. Nulla a imperdiet augue, vel fermentum " +
            "ipsum.",
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
      )
    )

    "Should create user" in {
      val game = baseGame.startGame(
        r,
        List(
          Maybe.Just("tom"),
          Maybe.Just("not really a good race"),
          Maybe.Just(Races.Human.getName)
        )
      )

      assert(game.isRight)
      assert(game.right.get.getPlayer.isJust)
    }
  }
}
