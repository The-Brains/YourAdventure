package thebrains.youradventure.Adventure.BodyPack

import scalaz.Maybe
import thebrains.youradventure.FactoriesTest.BodyPartPack._
import thebrains.youradventure.FactoriesTest.FEquipment
import thebrains.youradventure.ParentTest
import thebrains.youradventure.Utils.ToOption._
class PlayerBodyCollectionTest extends ParentTest {
  "PlayerBodyCollection" - {
    val bodyPartA = FPlayerBodyPart()
    val bodyPartB = unsafeRun(FPlayerBodyPartEquipped())
    val equipmentB = bodyPartB.getEquipment
    val collection = FPlayerBodyCollection(
      bodyParts = List[PlayerBodyPart](bodyPartA, bodyPartB).just
    )

    "Should return right length" in {
      assertEquals(2, collection.length)
    }

    "Should equip correct equipment" in {
      assertEquals(List(equipmentB), collection.equipments)
      assertEquals(Maybe.empty, collection.getEquipment(bodyPartA.getBodyPart))
      assertEquals(equipmentB.just, collection.getEquipment(bodyPartB.getBodyPart))

      val equipmentA = FEquipment(bodyPart = bodyPartA.getBodyPart.just)
      val result = unsafeRunToEither(collection.equip(equipmentA))

      assert(result.isRight)

      val newCollection = result.right.get
      assertEquals(List(equipmentA, equipmentB), newCollection.equipments)
      assertEquals(equipmentA.just, newCollection.getEquipment(bodyPartA.getBodyPart))
      assertEquals(equipmentB.just, newCollection.getEquipment(bodyPartB.getBodyPart))
    }

    "Should not equip bad equipment" in {
      val equipmentA = FEquipment()
      val result = unsafeRunToEither(collection.equip(equipmentA))
      assert(result.isLeft)
    }

    "Should replace equipment" in {
      assertEquals(List(equipmentB), collection.equipments)
      assertEquals(Maybe.empty, collection.getEquipment(bodyPartA.getBodyPart))
      assertEquals(equipmentB.just, collection.getEquipment(bodyPartB.getBodyPart))

      val equipmentBB = FEquipment(bodyPart = bodyPartB.getBodyPart.just)
      val result = unsafeRunToEither(collection.equip(equipmentBB))

      assert(result.isRight)

      val newCollection = result.right.get
      assertEquals(List(equipmentBB), newCollection.equipments)
      assertEquals(Maybe.empty, newCollection.getEquipment(bodyPartA.getBodyPart))
      assertEquals(equipmentBB.just, newCollection.getEquipment(bodyPartB.getBodyPart))
    }

  }
}
