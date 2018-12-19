package thebrains.youradventure.Adventure.TransformationPack

import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute._
import thebrains.youradventure.Adventure.AttributePack._
import thebrains.youradventure.Adventure.Error

case class Transformation(
  attribute:              Attribute,
  forwardTransformation:  AttributeTransformation,
  backwardTransformation: AttributeTransformation,
  value:                  AttributeType,
  operation:              Operation,
  modification:           Modification
) {
  override def toString: String = s"${attribute.toString} -> $operation $modification $value"

  def asCollection: TransformationCollection = TransformationCollection(this)

  def ++(other: Transformation): TransformationCollection = asCollection ++ other

  def ++(other: TransformationCollection): TransformationCollection = other ++ this

  private def execute(
    action:          AttributeTransformation,
    playerAttribute: PlayerAttribute
  ): Either[Error, PlayerAttribute] = {
    if (playerAttribute.attribute === attribute) {
      Right(
        playerAttribute.copy(
          value = action(playerAttribute.value)
        )
      )
    } else {
      Left(
        Error(
          "Wrong attribute for transformation",
          s"Transformation apply to ${attribute.toString}, not for ${playerAttribute.toString}"
        )
      )
    }
  }

  def >>(playerAttribute: PlayerAttribute): Either[Error, PlayerAttribute] = {
    execute(forwardTransformation, playerAttribute)
  }

  def revert(playerAttribute: PlayerAttribute): Either[Error, PlayerAttribute] = {
    execute(backwardTransformation, playerAttribute)
  }

  def canApply(playerAttribute: PlayerAttribute): Boolean = {
    playerAttribute.attribute === this.attribute
  }
}

sealed trait Modification

case object Increase extends Modification

case object Decrease extends Modification

sealed trait Operation

case object Addition extends Operation with FullOperation

case object Multiply extends Operation with FullOperation

sealed trait FullOperation

case object Reduce extends FullOperation

case object Divide extends FullOperation

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
        backwardTransformation = backward,
        value,
        operation,
        modify
      )
    }
  }

  case class TransformationWithValue(
    forwardTransformation:  AttributeTransformation,
    backwardTransformation: AttributeTransformation,
    value:                  AttributeType,
    operation:              Operation,
    modify:                 Modification
  ) {
    def onAttribute(attribute: Attribute): Transformation = {
      Transformation(
        attribute,
        forwardTransformation,
        backwardTransformation,
        value,
        operation,
        modify
      )
    }
  }

  def willDo(operation: FullOperation): TransformValue = {
    operation match {
      case Addition => willDo(Addition, Increase)
      case Multiply => willDo(Multiply, Increase)
      case Reduce   => willDo(Addition, Decrease)
      case Divide   => willDo(Multiply, Decrease)
    }
  }

  def willDo(
    operation: Operation,
    modify:    Modification
  ): TransformValue = {
    TransformValue(operation, modify)
  }
}
