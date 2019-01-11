package thebrains.youradventure.Adventure.ActionPack

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ConditionPack.Condition
import thebrains.youradventure.Adventure.StepPack.{StepCollection, Steps}
import thebrains.youradventure.FactoriesTest.ActionPack.{FAction, FActionCollection}
import thebrains.youradventure.FactoriesTest.Condition.{FCondition, FConditionOn}
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
        val playerStep = unsafeRunToEither(FPlayer().flatMap { p =>
          Actions.playerStatusMenu(p, _ => IO.sync(Steps.EmptyStep))
        })

        val action = playerStep.extract

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
        val step = exitStep.extract
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
      (a |+| b).shouldFail
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
        val output = unsafeRunToEither(io).extract
        assertEquals(Steps.EmptyStep.getName, output.getName)
      }

      "Should return the right step from string" in {
        val io = b.getStep(stepCollection)
        val output = unsafeRunToEither(io).extract
        assertEquals(Steps.ExitStep.getName, output.getName)
      }

      "Should fail if invalid step" in {
        FAction(targetStep = Maybe.just(Left("not a real step")))
          .getStep(stepCollection).shouldFail
      }
    }

    "Condition" - {
      val p = FPlayer().extract

      "Should be displayed by default" in {
        assert(a.canBeDisplayed(p).extract)
      }

      "Should be not displayed if condition invalid" in {
        val actionWithCondition = a.copy(conditions = a.getConditions :+ FCondition())
        assertFalse(actionWithCondition.canBeDisplayed(p).extract)
      }

      "Should be displayed if all condition are valid" in {
        val actionWithCondition = a.copy(
          conditions = a.getConditions :+
            Condition(FConditionOn.trueFor(p)) :+
            Condition(FConditionOn.trueFor(p))
        )
        assert(actionWithCondition.canBeDisplayed(p).extract)
      }
    }
  }
}
