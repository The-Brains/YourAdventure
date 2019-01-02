package thebrains.youradventure.Adventure.BodyPack

import thebrains.youradventure.Adventure.Equipment
import thebrains.youradventure.FactoriesTest.BodyPartPack.FPlayerBodyPart
import thebrains.youradventure.FactoriesTest.FEquipment
import thebrains.youradventure.ParentTest
import thebrains.youradventure.Utils.ToOption._

class PlayerBodyPartTest extends ParentTest {
  "PlayerBodyPart" - {
    val playerBodyPart = FPlayerBodyPart()

    "Equipment" - {
      "Should equip with good equipment" in {
        val equipment = FEquipment(bodyPart = playerBodyPart.getBodyPart.just)
        assert(playerBodyPart.canEquip(equipment))
        val result = unsafeRunToEither(playerBodyPart.equip(equipment))
        assert(result.isRight)
        val newBodyPart = result.right.get
        assertEquals(playerBodyPart.getBodyPart, newBodyPart.getBodyPart)
        assertEquals(equipment, newBodyPart.getEquipment)
      }

      "Should fail to equip with bad equipment" in {
        val equipment = FEquipment()
        assertFalse(playerBodyPart.canEquip(equipment))
        assert(unsafeRunToEither(playerBodyPart.equip(equipment)).isLeft)
      }

      "Can change equipment" in {
        def checkEquiping(
          bodyPart:  PlayerBodyPart,
          equipment: Equipment
        ): PlayerBodyPartEquipped = {
          assert(bodyPart.canEquip(equipment))
          val result = unsafeRunToEither(bodyPart.equip(equipment))
          assert(result.isRight)
          val newBodyPart: PlayerBodyPartEquipped = result.right.get
          assertEquals(bodyPart.getBodyPart, newBodyPart.getBodyPart)
          assertEquals(equipment, newBodyPart.getEquipment)
          newBodyPart
        }

        val equipment: Equipment = FEquipment(bodyPart = playerBodyPart.getBodyPart.just)
        val newBodyPart = checkEquiping(playerBodyPart, equipment)
        val equipment2 = FEquipment(bodyPart = playerBodyPart.getBodyPart.just)
        checkEquiping(newBodyPart, equipment2)

      }
    }
  }
}
