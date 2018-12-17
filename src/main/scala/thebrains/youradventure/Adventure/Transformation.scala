package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.PlayerAttribute._

class Transformation(
  attribute:              Attribute,
  forwardTransformation:  AttributeTransformation,
  backwardTransformation: AttributeTransformation
) {
  def applyOn(playerAttribute: PlayerAttribute): PlayerAttribute = {
    require(playerAttribute.attribute === attribute)
    playerAttribute.copy(
      value = forwardTransformation(playerAttribute.value)
    )
  }

  def revert(playerAttribute: PlayerAttribute): PlayerAttribute = {
    require(playerAttribute.attribute === attribute)
    playerAttribute.copy(
      value = backwardTransformation(playerAttribute.value)
    )
  }

  def canApply(playerAttribute: PlayerAttribute): Boolean = {
    playerAttribute.attribute === this.attribute
  }
}

sealed trait Modification

case object Increase extends Modification

case object Decrease extends Modification

sealed trait Operation

case object Addition extends Operation

case object Multiply extends Operation

object TransformationBuilder {

  case class TransformValue private (
    operation: Operation,
    modify:    Modification
  ) {
    private def positive(value: AttributeType)(attributeValue: AttributeType): AttributeType = {
      operation match {
        case Addition => attributeValue + value
        case Multiply => attributeValue * value
      }
    }

    private def negative(value: AttributeType)(attributeValue: AttributeType): AttributeType = {
      operation match {
        case Addition => attributeValue - value
        case Multiply => attributeValue / value
      }
    }

    private def transformations(
      value: AttributeType
    ): (AttributeTransformation, AttributeTransformation) = {
      modify match {
        case Increase => (positive(value), negative(value))
        case Decrease => (negative(value), positive(value))
      }
    }

    def byValueOf(value: AttributeType): TransformationWithValue = {
      val (forward, backward) = transformations(value)
      TransformationWithValue(
        forwardTransformation = forward,
        backwardTransformation = backward
      )
    }
  }

  case class TransformationWithValue(
    forwardTransformation:  AttributeTransformation,
    backwardTransformation: AttributeTransformation
  ) {
    def onAttribute(attribute: Attribute): Transformation = {
      new Transformation(
        attribute,
        forwardTransformation,
        backwardTransformation
      )
    }
  }

  def willDo(
    operation: Operation,
    modify:    Modification
  ): TransformValue = {
    TransformValue(operation, modify)
  }
}
