package thebrains.youradventure.Adventure.AttributePack

import scalaz.Maybe
import thebrains.youradventure.Adventure.TransformationPack._
import thebrains.youradventure.ParentTest
import thebrains.youradventure.Utils.ToOption._

class PlayerAttributeTest extends ParentTest {
  "PlayerAttribute" - {
    "as List" - {
      "With Transformation" in {
        val attributesIO =
          unsafeRunToEither(
            Attributes.Strength.toPlayerAttribute(10) ++
              Attributes.Constitution.toPlayerAttribute(10)
          )

        assert(attributesIO.isRight)
        val attributes = attributesIO.right.get

        val transformations: TransformationCollection =
          TransformationBuilder
            .willDo(Addition, Increase)
            .byValueOf(1).onAttribute(Attributes.Strength) ++
            TransformationBuilder
              .willDo(Addition, Decrease)
              .byValueOf(2).onAttribute(Attributes.Intelligence)

        val newAttributes = unsafeRun(attributes << transformations)

        assertEquals(11.just, newAttributes.getAttributeValue(Attributes.Strength))
        assertEquals(10.just, newAttributes.getAttributeValue(Attributes.Constitution))
        assertEquals(Maybe.Empty(), newAttributes.getAttributeValue(Attributes.Intelligence))
      }
    }
  }
}
