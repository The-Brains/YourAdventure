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
    ).right.get

    "Race condition" - {
      val condition = Condition(RaceCondition(Races.Human))

      "Should be true" in {
        val result = unsafeRunToEither(condition.isTrueFor(player))
        assert(result.isRight)
        val isCondition = result.right.get
        assert(isCondition)
      }
    }

    "AttributeCondition" - {
      val condition = Condition(
        AttributeCondition(
          attribute = Attributes.Constitution,
          minValue = 2,
          defaultWhenAttributeDoesNotExist = false
        )
      )

      "Should not be valid" in {
        val result = unsafeRunToEither(condition.isTrueFor(player))
        assert(result.isRight)
        assertEquals(false, result.right.get)
      }

      "Should be valid" in {
        val newPlayer = player.copy(
          baseAttributes =
            player.getBaseAttributes ++ Attributes.Constitution.toPlayerAttribute(10)
        )
        val result = unsafeRunToEither(condition.isTrueFor(newPlayer))
        assert(result.isRight)
        assertEquals(true, result.right.get)
      }
    }

    "Equipment" - {
      val bodyPart = player.getBodyParts.getItems.head.getBodyPart
      val equipment = FEquipment(bodyPart = bodyPart.just)
      val condition = Condition(
        EquipmentCondition(
          equipment = equipment
        )
      )

      "Should be valid" in {
        val newPlayer = unsafeRun(player.equipWild(equipment))
        val result = unsafeRunToEither(condition.isTrueFor(newPlayer))
        assert(result.isRight)
        assertEquals(true, result.right.get)
      }

      "Should not be valid" in {
        val result = unsafeRunToEither(condition.isTrueFor(player))
        assert(result.isRight)
        assertEquals(false, result.right.get)
      }
    }

    "Combination" - {
      val bodyPart = player.getBodyParts.getItems.head.getBodyPart
      val equipment = FEquipment(bodyPart = bodyPart.just)

      val condition = Condition(
        RaceCondition(Races.Human),
        AttributeCondition(
          attribute = Attributes.Constitution,
          minValue = 2,
          defaultWhenAttributeDoesNotExist = false
        )
      ) ++ EquipmentCondition(equipment = equipment)

      "Should be valid when all true" in {
        val newPlayer = unsafeRun(
          player
            .copy(
              baseAttributes =
                player.getBaseAttributes ++ Attributes.Constitution.toPlayerAttribute(10)
            )
            .equipWild(equipment)
        )
        val result = unsafeRunToEither(condition.isTrueFor(newPlayer))
        assert(result.isRight)
        assertEquals(true, result.right.get)
      }

      "Should not be valid if any is wrong" in {
        val result = unsafeRunToEither(condition.isTrueFor(player))
        assert(result.isRight)
        assertEquals(false, result.right.get)
      }
    }
  }
}
