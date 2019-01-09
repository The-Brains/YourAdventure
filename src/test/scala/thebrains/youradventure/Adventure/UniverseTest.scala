package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.StepPack.{StepCollection, Steps}
import thebrains.youradventure.FactoriesTest.FLocation
import thebrains.youradventure.FactoriesTest.StepPack.FStepToStep
import thebrains.youradventure.ParentTest
import thebrains.youradventure.Utils.ToOption._

import scala.collection.immutable.SortedSet

class UniverseTest extends ParentTest {
  "Universe" - {
    "Creation" - {
      "Should fail if step are not unique" in {
        val location1 = FLocation()
        val location2 = FLocation()
        val step1 = FStepToStep(
          location = location1.just
        )
        val step2 = FStepToStep(
          location = location2.just,
          targetSteps = List(Left(step1.getName))
        )
        val step3 = FStepToStep(
          location = location1.just,
          targetSteps = List(Left(step1.getName), Right(step2))
        )

        Universe(
          availableRaces = List(Races.Human),
          availableSteps = StepCollection(step1, step1),
          startingStep = step3
        ).shouldFail
      }
    }
    "Location" - {
      "Should return the right locations when one step" in {
        val universeIO = unsafeRunToEither(
          Universe(
            availableRaces = List(Races.Human),
            availableSteps = StepCollection.Empty,
            startingStep = Steps.ExitStep
          )
        )

        val universe = universeIO.extract
        val locationsIO = unsafeRunToEither(universe.availableLocations)
        val locations = locationsIO.extract
        assertEquals(SortedSet(Steps.ExitStep.getLocation), locations)
      }

      "Should return the right locations when several steps" in {
        val location1 = FLocation()
        val location2 = FLocation()
        val step1 = FStepToStep(
          location = location1.just
        )
        val step2 = FStepToStep(
          location = location2.just,
          targetSteps = List(Left(step1.getName))
        )
        val step3 = FStepToStep(
          location = location1.just,
          targetSteps = List(Left(step1.getName), Right(step2))
        )

        val universeIO = unsafeRunToEither(
          Universe(
            availableRaces = List(Races.Human),
            availableSteps = StepCollection(step1),
            startingStep = step3
          )
        )

        val universe = universeIO.extract
        val locationsIO = unsafeRunToEither(universe.availableLocations)
        val locations = locationsIO.extract
        assertEquals(SortedSet(location1, location2, Steps.ExitStep.getLocation), locations)
      }
    }

    "AllSteps" - {
      "Should return the right locations when one step" in {
        val universeIO = unsafeRunToEither(
          Universe(
            availableRaces = List(Races.Human),
            availableSteps = StepCollection.Empty,
            startingStep = Steps.ExitStep
          )
        )

        val universe = universeIO.extract
        val stepsIO = unsafeRunToEither(universe.getAllSteps)
        val steps = stepsIO.extract
        assertEquals(SortedSet(Steps.ExitStep), steps)
      }

      "Should return the right locations when several steps" in {
        val location1 = FLocation()
        val location2 = FLocation()
        val step1 = FStepToStep(
          location = location1.just
        )
        val step2 = FStepToStep(
          location = location2.just,
          targetSteps = List(Left(step1.getName))
        )
        val step3 = FStepToStep(
          location = location1.just,
          targetSteps = List(Left(step1.getName), Right(step2))
        )

        val universeIO = unsafeRunToEither(
          Universe(
            availableRaces = List(Races.Human),
            availableSteps = StepCollection(step1),
            startingStep = step3
          )
        )

        val universe = universeIO.extract
        val stepsIO = unsafeRunToEither(universe.getAllSteps)
        val steps = stepsIO.extract
        assertEquals(SortedSet(step1, step2, step3, Steps.ExitStep), steps)
      }
    }
  }
}
