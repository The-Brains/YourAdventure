package thebrains.youradventure.Adventure.BodyPack

import thebrains.youradventure.Adventure.Equipment
import thebrains.youradventure.FactoriesTest.BodyPartPack.FPlayerBodyPart
import thebrains.youradventure.FactoriesTest.FEquipment
import thebrains.youradventure.ParentTest
import thebrains.youradventure.Utils.ToOption._

class PlayerBodyPartTest extends ParentTest {
  "PlayerBodyPart" - {
    val playerBodyPart = FPlayerBodyPart()

    def checkEquipping(
      bodyPart:  PlayerBodyPart,
      equipment: Equipment
    ): PlayerBodyPartEquipped = {
      assert(bodyPart.canEquip(equipment))
      val result = unsafeRunToEither(bodyPart.equip(equipment))
      val newBodyPart: PlayerBodyPartEquipped = result.extract
      assertEquals(bodyPart.getBodyPart, newBodyPart.getBodyPart)
      assertEquals(equipment, newBodyPart.getEquipment)
      assert(newBodyPart.isWearing)
      newBodyPart
    }

    "Equipment" - {
      "Should equip with good equipment" in {
        val equipment = FEquipment(bodyPart = playerBodyPart.getBodyPart.just)
        checkEquipping(playerBodyPart, equipment)
      }

      "Should fail to equip with bad equipment" in {
        val equipment = FEquipment()
        assertFalse(playerBodyPart.canEquip(equipment))
        playerBodyPart.equip(equipment).shouldFail
      }

      "Can change equipment" in {
        val equipment: Equipment = FEquipment(bodyPart = playerBodyPart.getBodyPart.just)
        val newBodyPart = checkEquipping(playerBodyPart, equipment)
        val equipment2 = FEquipment(bodyPart = playerBodyPart.getBodyPart.just)
        checkEquipping(newBodyPart, equipment2)
      }
    }

    "Json" - {
      "Should contains name" in {
        assert(playerBodyPart.toString.contains(playerBodyPart.getName))
      }

      "Should contains equipment if equipped" in {
        val equipment = FEquipment(bodyPart = playerBodyPart.getBodyPart.just)
        val newBodyPart = checkEquipping(playerBodyPart, equipment)
        assert(newBodyPart.toString.contains(equipment.getName))
      }
    }
  }
}
