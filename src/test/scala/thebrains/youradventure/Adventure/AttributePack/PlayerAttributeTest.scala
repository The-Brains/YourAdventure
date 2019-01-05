package thebrains.youradventure.Adventure.AttributePack

import thebrains.youradventure.FactoriesTest.AttributePack.FAttribute
import thebrains.youradventure.ParentTest

class PlayerAttributeTest extends ParentTest {
  "PlayerAttribute" - {
    val a = FAttribute()
    "Should be equals to itself" in {
      assert(a === a)
    }

    "Should not be equals to others" in {
      assertFalse(Attributes.Strength === Attributes.Constitution)
    }

    "Merge" - {
      "Should merge correctly" in {
        val pA = a.toPlayerAttribute(10)
        val pB = a.toPlayerAttribute(2)

        val result = unsafeRunToEither(pA |+| pB)
        assert(result.isRight)

        val newPlayerAttribute = result.right.get
        assertEquals(a.getName, newPlayerAttribute.getName)
        assertEquals(pA.getName, newPlayerAttribute.getName)
        assertEquals(pB.getName, newPlayerAttribute.getName)
        assertEquals(pA.getValue + pB.getValue, newPlayerAttribute.getValue)
      }
    }
  }
}
