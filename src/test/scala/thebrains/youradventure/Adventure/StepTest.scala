package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.Adventure.AttributePack.Attributes
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.ParentTest
import thebrains.youradventure.TerminalUIPack._

class StepTest extends ParentTest {
  "Step" - {
    lazy val step = new Step(
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
      location = Location.Earth,
      transformations = TransformationCollection(
        TransformationBuilder
          .willDo(Addition)
          .byValueOf(10)
          .onAttribute(Attributes.Strength)
      ),
      availableActions = ActionCollection("Whats up?")(
        Action("Look", "Look at Earth closer", Step.EmptyStep),
        Action("Leave", "Leave earth alone", Step.EmptyStep)
      )
    )

    "Display" - {
      Renderer(TerminalPrint())
        .display(step)
        .printToConsole()
    }
  }
}
