package thebrains.youradventure.Adventure.BodyPack

import thebrains.youradventure.FactoriesTest.BodyPartPack.FBodyPart
import thebrains.youradventure.FactoriesTest.RawFactory
import thebrains.youradventure.ParentTest
import thebrains.youradventure.Utils.ToOption._

class BodyPartTest extends ParentTest {
  "BodyPart" - {
    val name = "A"
    val descriptor = "A".just
    val bodyPart = FBodyPart(
      name = name.just,
      descriptor = descriptor.just
    )

    "SamePart" - {
      "Should be same part with same name" in {
        assert(bodyPart samePartAs FBodyPart(name = name.just))
        assert(bodyPart samePartAs FBodyPart(name = name.just, descriptor = descriptor.just))
      }

      "Should not be same part with different name" in {
        assertFalse(bodyPart samePartAs FBodyPart())
        assertFalse(bodyPart samePartAs FBodyPart(descriptor = descriptor.just))
      }
    }

    "SameExactPart" - {
      "Should be equals if name and descriptor" in {
        assert(
          bodyPart sameExactPartAs FBodyPart(
            name = name.just,
            descriptor = descriptor.just
          )
        )
      }

      "Should not be equals otherwise" in {
        assertFalse(bodyPart sameExactPartAs FBodyPart(name = name.just))
        assertFalse(bodyPart sameExactPartAs FBodyPart())
        assertFalse(bodyPart sameExactPartAs FBodyPart(descriptor = descriptor.just))
      }
    }

    "Set" - {
      "Should keep the right body parts" in {
        import thebrains.youradventure.Adventure.CollectionPack.ToSortedSet._
        val bodyPartB = FBodyPart(
          name = "B".just,
          descriptor = descriptor.just
        )
        val bodyPartC = FBodyPart(
          name = "C".just
        )
        val bodyPartD = FBodyPart(
          name = name.just,
          descriptor = descriptor.just
        )
        val bodyPartE = FBodyPart(
          name = name.just,
          descriptor = "E".just.just
        )
        val bodyPartF = FBodyPart(
          name = name.just,
          descriptor = "F".just.just
        )

        assertEquals(
          List(bodyPart, bodyPartB, bodyPartC, bodyPartE, bodyPartF).sorted,
          List(
            bodyPart,
            bodyPartB,
            bodyPartC,
            bodyPartC,
            bodyPartD,
            bodyPartE,
            bodyPartE,
            bodyPart,
            bodyPartB,
            bodyPartD,
            bodyPartF,
            bodyPartF
          ).toSortedSet.toList
        )
      }
    }
  }
}
