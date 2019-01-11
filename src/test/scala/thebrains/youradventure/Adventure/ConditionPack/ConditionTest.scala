package thebrains.youradventure.Adventure.ConditionPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.AttributePack.Attributes
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FactoriesTest.{FEquipment, FPlayer}
import thebrains.youradventure.ParentTest
import thebrains.youradventure.Utils.ToOption._

class ConditionTest extends ParentTest {
  "Condition" - {
    val player = unsafeRunToEither(
      FPlayer(race = Maybe.just(Races.Human))
    ).extract

    "Race condition" - {
      val condition = Condition(Condition.create.forRace(Races.Human))

      "Should be true" in {
        val result = unsafeRunToEither(condition.isTrueFor(player))
        val isCondition = result.extract
        assert(isCondition)
      }
    }

    "AttributeCondition" - {
      val condition = Condition(
        Condition.create.forAttribute(
          attribute = Attributes.Constitution,
          minValue = 2,
          defaultWhenAttributeDoesNotExist = false
        )
      )

      "Should not be valid" in {
        val result = unsafeRunToEither(condition.isTrueFor(player)).extract
        assertEquals(false, result)
      }

      "Should be valid" in {
        val newPlayer = player.copy(
          baseAttributes =
            player.getBaseAttributes ++ Attributes.Constitution.toPlayerAttribute(10)
        )
        val result = unsafeRunToEither(condition.isTrueFor(newPlayer)).extract
        assertEquals(true, result)
      }
    }

    "Equipment" - {
      val bodyPart = player.getBodyParts.getItems.head.getBodyPart
      val equipment = FEquipment(bodyPart = bodyPart.just)
      val condition = Condition(
        Condition.create.forEquipment(
          equipment = equipment
        )
      )

      "Should be valid" in {
        val newPlayer = unsafeRun(player.equipWild(equipment))
        val result = unsafeRunToEither(condition.isTrueFor(newPlayer)).extract
        assertEquals(true, result)
      }

      "Should not be valid" in {
        val result = unsafeRunToEither(condition.isTrueFor(player)).extract
        assertEquals(false, result)
      }
    }

    "Combination" - {
      val bodyPart = player.getBodyParts.getItems.head.getBodyPart
      val equipment = FEquipment(bodyPart = bodyPart.just)

      val condition = Condition(
        Condition.create.forRace(Races.Human),
        Condition.create.forAttribute(
          attribute = Attributes.Constitution,
          minValue = 2,
          defaultWhenAttributeDoesNotExist = false
        )
      ) ++ Condition.create.forEquipment(equipment = equipment)

      "Should be valid when all true" in {
        val newPlayer = unsafeRun(
          player
            .copy(
              baseAttributes =
                player.getBaseAttributes ++ Attributes.Constitution.toPlayerAttribute(10)
            )
            .equipWild(equipment)
        )
        val result = unsafeRunToEither(condition.isTrueFor(newPlayer)).extract
        assertEquals(true, result)
      }

      "Should not be valid if any is wrong" in {
        val result = unsafeRunToEither(condition.isTrueFor(player))
        val resultValue = result.extract
        assertEquals(false, resultValue)
      }
    }
  }
}
