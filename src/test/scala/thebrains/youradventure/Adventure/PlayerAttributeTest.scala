package thebrains.youradventure.Adventure

import scalaz.Maybe
import thebrains.youradventure.Adventure.AttributePack.{AttributeCollection, Attributes}
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.ParentTest

class PlayerAttributeTest extends ParentTest {
  "PlayerAttribute" - {
    "as List" - {
      "With Transformation" in {
        val attributes: AttributeCollection =
          unsafeRun(Attributes.Strength.toPlayerAttribute(10) ++
            Attributes.Constitution.toPlayerAttribute(10))

        val transformations: TransformationCollection =
          TransformationBuilder
            .willDo(Addition, Increase)
            .byValueOf(1).onAttribute(Attributes.Strength) ++
            TransformationBuilder
              .willDo(Addition, Decrease)
              .byValueOf(2).onAttribute(Attributes.Intelligence)

        val newAttributes = unsafeRun(attributes << transformations)

        assertEquals(Maybe.Just(11), newAttributes.getAttributeValue(Attributes.Strength))
        assertEquals(Maybe.Just(10), newAttributes.getAttributeValue(Attributes.Constitution))
        assertEquals(Maybe.Empty(), newAttributes.getAttributeValue(Attributes.Intelligence))
      }
    }
  }
}
