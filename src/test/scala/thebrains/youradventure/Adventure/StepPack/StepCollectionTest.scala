package thebrains.youradventure.Adventure.StepPack

import thebrains.youradventure.FactoriesTest.StepPack.FStep
import thebrains.youradventure.ParentTest

class StepCollectionTest extends ParentTest {
  "StepCollection" - {
    "retrieve" - {
      val step = FStep()
      val stepCollection = StepCollection(step, FStep(), FStep(), FStep())
      "Should return the right step" in {
        val stepReturned = stepCollection.getStep(step.getName).extract
        assertEquals(stepReturned.getName, step.getName)
      }

      "Should return error if not found" in {
        stepCollection.getStep("This does not exist").shouldFail
      }
    }

    "merge" - {
      val stepCollection = StepCollection(List(FStep()))

      "Should be 1 at first" in {
        assertEquals(1, stepCollection.length)
      }

      "Should merge step" in {
        assertEquals(2, (stepCollection ++ FStep()).length)
        assertEquals(2, (stepCollection :+ FStep()).length)
      }
    }

    "empty" - {
      "Should be length 0" in {
        class CollectionTest extends StepCollection(steps = List(FStep(), FStep())) {
          def emptyPublic: StepCollection = this.empty
        }

        val collection: CollectionTest = new CollectionTest()
        assertEquals(0, collection.emptyPublic.length)
      }
    }
  }
}
