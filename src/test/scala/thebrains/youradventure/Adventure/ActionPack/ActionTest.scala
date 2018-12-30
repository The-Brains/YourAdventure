package thebrains.youradventure.Adventure.ActionPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.StepPack.{StepCollection, Steps}
import thebrains.youradventure.ParentTest

class ActionTest extends ParentTest {
  "Action" - {
    val a = Action("action A name", "action A description", Steps.EmptyStep, Nil)
    val b = Action("action B name", "action B description", "Exit", Nil)

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
      assertEquals(Maybe.just(a.getName), collection.get(0).map(_.getName))
      assertEquals(Maybe.just(b.getName), collection.get(1).map(_.getName))
      assertEquals(Maybe.empty, collection.get(2).map(_.getName))
    }

    "Should not combine" in {
      val io = a |+| b
      assert(unsafeRunToEither(io).isLeft)
    }

    "Interaction with collection" - {
      val question = "question"
      val collection = a ++ ActionCollection(question)(b)

      "Should assemble with collection" in {
        assertEquals(Maybe.just(question), collection.getQuestion)
        assertEquals(Maybe.just(a.getName), collection.get(0).map(_.getName))
        assertEquals(Maybe.just(b.getName), collection.get(1).map(_.getName))
        assertEquals(Maybe.empty, collection.get(2).map(_.getName))
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
        val c = Action("action C name", "action C description", Left("not a real step"))
        val io = c.getStep(stepCollection)
        val output = unsafeRunToEither(io)
        assert(output.isLeft)
      }
    }
  }
}
