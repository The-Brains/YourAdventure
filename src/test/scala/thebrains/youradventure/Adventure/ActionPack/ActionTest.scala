package thebrains.youradventure.Adventure.ActionPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.StepPack.{StepCollection, Steps}
import thebrains.youradventure.FactoriesTest.ActionPack.{FAction, FActionCollection}
import thebrains.youradventure.FactoriesTest.FPlayer
import thebrains.youradventure.ParentTest
import thebrains.youradventure.Utils.ToOption._

class ActionTest extends ParentTest {
  "Action" - {
    val a = FAction(
      targetStep = Maybe.just(Right(Steps.EmptyStep))
    )
    val b = FAction(
      targetStep = Maybe.just(Left("Exit"))
    )

    "Player step" - {
      "Should create step player" in {
        val playerStep = unsafeRunToEither(FPlayer().map { p =>
          Actions.playerStatusMenu(p, _ => Steps.EmptyStep)
        })
        assert(playerStep.isRight)

        val action = playerStep.right.get
        assert(action.getTargetStep.isRight)

        val step = action.getTargetStep.right.get
        assertEquals(Steps.EmptyStep.getName, step.getName)
      }
    }

    "Exit step" - {
      val e = Actions.Exit
      "Should be the exit step" in {
        val io = e.getStep(StepCollection(Steps.ExitStep))
        val exitStep = unsafeRunToEither(io)
        assert(exitStep.isRight)
        val step = exitStep.right.get
        assertEquals(Steps.ExitStep.getName, step.getName)
      }
    }

    "Should assemble" in {
      val collection = a ++ b
      assertEquals(2, collection.length)
      assertEquals(a.getName.just, collection.get(0).map(_.getName))
      assertEquals(b.getName.just, collection.get(1).map(_.getName))
      assertEquals(Maybe.empty, collection.get(2).map(_.getName))
    }

    "Should not combine" in {
      val io = a |+| b
      assert(unsafeRunToEither(io).isLeft)
    }

    "Interaction with collection" - {
      val startingCollection = FActionCollection(lengthAction = 1.just)
      val collection = a ++ b ++ startingCollection
      val indexOfEmptyAction = 3

      "Should assemble with collection" in {
        assertEquals(startingCollection.getQuestion, collection.getQuestion)
        assertEquals(a.getName.just, collection.get(0).map(_.getName))
        assertEquals(b.getName.just, collection.get(1).map(_.getName))
        assert(collection.get(2).isJust)
        assert(collection.get(indexOfEmptyAction).isEmpty)
      }
    }

    "Get step" - {
      val stepCollection = StepCollection(Steps.EmptyStep, Steps.ExitStep)
      "Should return the right step" in {
        val io = a.getStep(stepCollection)
        val output = unsafeRunToEither(io)
        assert(output.isRight)
        assertEquals(Steps.EmptyStep.getName, output.right.get.getName)
      }

      "Should return the right step from string" in {
        val io = b.getStep(stepCollection)
        val output = unsafeRunToEither(io)
        assert(output.isRight)
        assertEquals(Steps.ExitStep.getName, output.right.get.getName)
      }

      "Should fail if invalid step" in {
        val c = FAction(targetStep = Maybe.just(Left("not a real step")))
        val io = c.getStep(stepCollection)
        val output = unsafeRunToEither(io)
        assert(output.isLeft)
      }
    }
  }
}
