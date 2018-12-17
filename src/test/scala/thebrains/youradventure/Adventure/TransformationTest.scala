package thebrains.youradventure.Adventure

import thebrains.youradventure.ParentTest

class TransformationTest extends ParentTest {
  "Transformation" - {
    val attribute = Attribute(
      name = "attack",
      description = "damage you do"
    )

    "Add" - {
      "Increase" - {
        val transformation = TransformationBuilder
          .willDo(Addition, Increase)
          .byValueOf(10)
          .onAttribute(attribute)

        "apply" in {
          val playerAttribute = attribute.toPlayerAttribute(10)
          val expected = 20
          val result = transformation.applyOn(playerAttribute)
          assertEquals(expected, result.value)
        }

        "revert" in {
          val playerAttribute = attribute.toPlayerAttribute(10)
          val tmp = transformation.applyOn(playerAttribute)
          val revert = transformation.revert(tmp)
          assertEquals(playerAttribute.value, revert.value)
        }
      }

      "Decrease" - {
        val transformation = TransformationBuilder
          .willDo(Addition, Decrease)
          .byValueOf(10)
          .onAttribute(attribute)

        "apply" in {
          val playerAttribute = attribute.toPlayerAttribute(10)
          val expected = 0
          val result = transformation.applyOn(playerAttribute)
          assertEquals(expected, result.value)
        }

        "revert" in {
          val playerAttribute = attribute.toPlayerAttribute(10)
          val tmp = transformation.applyOn(playerAttribute)
          val revert = transformation.revert(tmp)
          assertEquals(playerAttribute.value, revert.value)
        }
      }
    }

    "Multiply" - {
      "Increase" - {
        val transformation = TransformationBuilder
          .willDo(Multiply, Increase)
          .byValueOf(10)
          .onAttribute(attribute)

        "apply" in {
          val playerAttribute = attribute.toPlayerAttribute(10)
          val expected = 100
          val result = transformation.applyOn(playerAttribute)
          assertEquals(expected, result.value)
        }

        "revert" in {
          val playerAttribute = attribute.toPlayerAttribute(10)
          val tmp = transformation.applyOn(playerAttribute)
          val revert = transformation.revert(tmp)
          assertEquals(playerAttribute.value, revert.value)
        }
      }

      "Decrease" - {
        val transformation = TransformationBuilder
          .willDo(Multiply, Decrease)
          .byValueOf(10)
          .onAttribute(attribute)

        "apply" in {
          val playerAttribute = attribute.toPlayerAttribute(10)
          val expected = 1
          val result = transformation.applyOn(playerAttribute)
          assertEquals(expected, result.value)
        }

        "revert" in {
          val playerAttribute = attribute.toPlayerAttribute(10)
          val tmp = transformation.applyOn(playerAttribute)
          val revert = transformation.revert(tmp)
          assertEquals(playerAttribute.value, revert.value)
        }
      }
    }
  }
}
