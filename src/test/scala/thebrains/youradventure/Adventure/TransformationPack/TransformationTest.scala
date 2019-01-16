package thebrains.youradventure.Adventure.TransformationPack

import org.scalactic.source.Position
import org.scalatest.Assertion
import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute.AttributeType
import thebrains.youradventure.FactoriesTest.AttributePack.FAttribute
import thebrains.youradventure.FactoriesTest.TransformationPack.FTransformation
import thebrains.youradventure.ParentTest

class TransformationTest extends ParentTest {
  "Transformation" - {
    "Execute" - {
      "Should fail with wrong attribute" in {
        val attribute = FAttribute().toPlayerAttribute(10)
        val t = FTransformation()
        assertFalse(t.canApply(attribute))
        t.appliedTo(attribute).shouldFail
      }
    }

    "Operations" - {
      val attribute = FAttribute()

      def testApply(
        transformation: TransformationCollection,
        initValue:      AttributeType,
        expectedValue:  AttributeType
      )(
        implicit pos: Position
      ): Assertion = {
        val playerAttribute = attribute.toPlayerAttribute(initValue)
        val result = unsafeRunSync(transformation.applyOn(playerAttribute)).toEither
        val value = result.extract
        assertEquals(expectedValue, value.value)
      }

      def testRevert(
        transformation: TransformationCollection
      )(
        implicit pos: Position
      ): Assertion = {
        val playerAttribute = attribute.toPlayerAttribute(10)
        val tmp = unsafeRunSync(transformation.applyOn(playerAttribute)).toEither
        val tmpOut = tmp.extract
        val revert = unsafeRunSync(transformation.revert(tmpOut)).toEither
        val revertOut = revert.extract
        assertEquals(playerAttribute.value, revertOut.value)
      }

      "Add" - {
        "Increase" - {
          val transformation = TransformationBuilder
            .willDo(Addition)
            .byValueOf(10)
            .onAttribute(attribute)
            .asCollection

          "apply" in {
            testApply(transformation, 10, 20)
          }

          "revert" in {
            testRevert(transformation)
          }
        }

        "Decrease" - {
          val transformation = TransformationBuilder
            .willDo(Reduce)
            .byValueOf(10)
            .onAttribute(attribute)
            .asCollection

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
            .willDo(Multiply)
            .byValueOf(10)
            .onAttribute(attribute)
            .asCollection

          "apply" in {
            testApply(transformation, 10, 100)
          }

          "revert" in {
            testRevert(transformation)
          }
        }

        "Decrease" - {
          val transformation = TransformationBuilder
            .willDo(Divide)
            .byValueOf(10)
            .onAttribute(attribute)
            .asCollection

          "apply" in {
            testApply(transformation, 10, 1)
          }

          "revert" in {
            testRevert(transformation)
          }
        }
      }

      "Combination" - {
        "addition + addition" - {
          val transformation1 = TransformationBuilder
            .willDo(Addition)
            .byValueOf(10)
            .onAttribute(attribute)

          val transformation2 = TransformationBuilder
            .willDo(Addition)
            .byValueOf(20)
            .onAttribute(attribute)

          val transformation = transformation1 ++ transformation2

          "Should compute the right result" in {
            testApply(transformation, 10, 40)
            testRevert(transformation)
          }
        }

        "addition + reduction" - {
          val transformation1 = TransformationBuilder
            .willDo(Addition)
            .byValueOf(10)
            .onAttribute(attribute)

          val transformation2 = TransformationBuilder
            .willDo(Reduce)
            .byValueOf(20)
            .onAttribute(attribute)

          val transformation = transformation1 ++ transformation2

          "Should compute the right result" in {
            testApply(transformation, 40, 30)
            testRevert(transformation)
          }
        }

        "multiply + multiply" - {
          val transformation1 = TransformationBuilder
            .willDo(Multiply)
            .byValueOf(10)
            .onAttribute(attribute)

          val transformation2 = TransformationBuilder
            .willDo(Multiply)
            .byValueOf(10)
            .onAttribute(attribute)

          val transformation = transformation1 ++ transformation2

          "Should compute the right result" in {
            testApply(transformation, 10, 1000)
            testRevert(transformation)
          }
        }
      }
    }
  }
}
