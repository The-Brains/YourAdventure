package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.Attribute.{AttributeCollection, Attributes}
import thebrains.youradventure.Adventure.Transformation._
import thebrains.youradventure.ParentTest

class PlayerAttributeTest extends ParentTest {
  "PlayerAttribute" - {
    "as List" - {
      "With Transformation" in {
        val attributes: AttributeCollection =
          Attributes.Strength.toPlayerAttribute(10) ++
            Attributes.Constitution.toPlayerAttribute(10)

        val transformations: TransformationCollection =
          TransformationBuilder
            .willDo(Addition, Increase)
            .byValueOf(1).onAttribute(Attributes.Strength) ++
            TransformationBuilder
              .willDo(Addition, Decrease)
              .byValueOf(2).onAttribute(Attributes.Intelligence)

        val newAttributes = attributes << transformations

        assertEquals(Some(11), newAttributes.getAttributeValue(Attributes.Strength))
        assertEquals(Some(10), newAttributes.getAttributeValue(Attributes.Constitution))
        assertEquals(None, newAttributes.getAttributeValue(Attributes.Intelligence))
      }
    }
  }
}
