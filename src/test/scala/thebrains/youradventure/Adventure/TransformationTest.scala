package thebrains.youradventure.Adventure

import org.scalatest.Assertion
import thebrains.youradventure.Adventure.Attribute.{Attribute, Attributes}
import thebrains.youradventure.Adventure.Attribute.PlayerAttribute.AttributeType
import thebrains.youradventure.Adventure.Transformation._
import thebrains.youradventure.ParentTest

class TransformationTest extends ParentTest {
  "Transformation" - {
    val attribute = Attributes.Strength

    def testApply(
      transformation: Transformation,
      initValue:      AttributeType,
      expectedValue:  AttributeType
    ): Assertion = {
      val playerAttribute = attribute.toPlayerAttribute(initValue)
      val result = transformation.>>(playerAttribute)
      assert(result.isRight)
      assertEquals(expectedValue, result.right.get.value)
    }

    def testRevert(transformation: Transformation): Assertion = {
      val playerAttribute = attribute.toPlayerAttribute(10)
      val tmp = transformation.>>(playerAttribute)
      assert(tmp.isRight)

      val revert = transformation.revert(tmp.right.get)
      assert(revert.isRight)

      assertEquals(playerAttribute.value, revert.right.get.value)
    }

    "Add" - {
      "Increase" - {
        val transformation: Transformation = TransformationBuilder
          .willDo(Addition, Increase)
          .byValueOf(10)
          .onAttribute(attribute)

        "apply" in {
          testApply(transformation, 10, 20)
        }

        "revert" in {
          testRevert(transformation)
        }
      }

      "Decrease" - {
        val transformation = TransformationBuilder
          .willDo(Addition, Decrease)
          .byValueOf(10)
          .onAttribute(attribute)

        "apply" in {
          testApply(transformation, 10, 0)
        }

        "revert" in {
          testRevert(transformation)
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
          testApply(transformation, 10, 100)
        }

        "revert" in {
          testRevert(transformation)
        }
      }

      "Decrease" - {
        val transformation = TransformationBuilder
          .willDo(Multiply, Decrease)
          .byValueOf(10)
          .onAttribute(attribute)

        "apply" in {
          testApply(transformation, 10, 1)
        }

        "revert" in {
          testRevert(transformation)
        }
      }
    }
  }
}