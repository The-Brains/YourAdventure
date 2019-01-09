package thebrains.youradventure.Adventure.StepPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.ActionPack.Actions
import thebrains.youradventure.Adventure.Player
import thebrains.youradventure.FactoriesTest.ActionPack.FActionCollection
import thebrains.youradventure.FactoriesTest.FPlayer
import thebrains.youradventure.FactoriesTest.StepPack.FStep
import thebrains.youradventure.ParentTest
import thebrains.youradventure.Utils.ToOption._

class StepTest extends ParentTest {
  "Step" - {
    "No action" - {
      val step = FStep()

      "Should have no actions" in {
        val io = unsafeRunToEither(step.getActions(Maybe.empty[Player]))
        val result = io.extract
        assertEquals(0, result.length)
      }

      "Should have no actions even with Player" in {
        val player = unsafeRunToEither(FPlayer()).extract
        val io = unsafeRunToEither(step.getActions(player))
        val result = io.extract
        assertEquals(0, result.length)
      }
    }

    "With actions" - {
      val baseActionLength = 2
      val step = FStep(
        availableActions = FActionCollection(
          lengthAction = baseActionLength.just
        )
      )

      "Should have no actions" in {
        val io = unsafeRunToEither(step.getActions(Maybe.empty[Player]))
        val result = io.extract
        val expected = baseActionLength + 1
        assertEquals(expected, result.length)
      }

      "Should have no actions even with Player" in {
        val player = unsafeRunToEither(FPlayer()).extract
        val io = unsafeRunToEither(step.getActions(player))
        val result = io.extract
        val expected = baseActionLength + 2
        assertEquals(expected, result.length)
      }

      "Should have no actions even with Maybe[Player]" in {
        val player = unsafeRunToEither(FPlayer()).extract
        val io = unsafeRunToEither(step.getActions(player.just))
        val result = io.extract
        val expected = baseActionLength + 2
        assertEquals(expected, result.length)
      }
    }

    "From the player status itself" - {
      val baseStep = {
        val baseActionLength = 5
        FStep(availableActions = FActionCollection(lengthAction = baseActionLength.just))
      }
      val player = unsafeRunToEither(FPlayer()).extract
      val step = unsafeRunToEither(for {
        s1 <- Steps.playerStatusStep(baseStep, player)
        s2 <- Steps.playerStatusStep(s1, player)
        s  <- Steps.playerStatusStep(s2, player)
      } yield {
        s
      }).extract

      "Should have no actions" in {
        val io = unsafeRunToEither(step.getActions(Maybe.empty[Player]))
        val result = io.extract
        val expected = 2
        assertEquals(expected, result.length)
      }

      "Should have no actions even with Player" in {
        val io = unsafeRunToEither(step.getActions(player))
        val result = io.extract
        val expected = 3
        assertEquals(expected, result.length)
      }

      "Should have no actions even with Maybe[Player]" in {
        val player = unsafeRunToEither(FPlayer()).extract
        val io = unsafeRunToEither(step.getActions(player.just))
        val result = io.extract
        val expected = 3
        assertEquals(expected, result.length)
      }

      "Should have original step as action available" in {
        val actions = step.getActions(player)
        val actionFetched = unsafeRunToEither(actions).extract
        val expectedLength = 3
        assertEquals(expectedLength, actionFetched.length)

        val foundAction = actions.flatMap(_.getAction(Actions.Back(Steps.EmptyStep).getName))
        val foundActionEi = unsafeRunToEither(foundAction)
        val fetchingStep = foundActionEi.extract
        val stepRetrieved = fetchingStep.getTargetStep
        assert(stepRetrieved.isRight)
        val fetchedStep = stepRetrieved.right.get
        assertEquals(baseStep.getName, fetchedStep.getName)
      }
    }
  }
}
