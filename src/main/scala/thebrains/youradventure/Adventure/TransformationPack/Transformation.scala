package thebrains.youradventure.Adventure.TransformationPack

import scalaz.zio.IO
import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute._
import thebrains.youradventure.Adventure.AttributePack._
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Utils.Error
import thebrains.youradventure.Utils.BirdUtils.BirdOperator._

case class Transformation(
  attribute:              Attribute,
  forwardTransformation:  AttributeTransformation,
  backwardTransformation: AttributeTransformation,
  value:                  AttributeType,
  operation:              Operation,
  modification:           Modification
) extends AssemblyItemTrait(
      attribute.getName,
      attribute.getDescription
    ) {
  override def toString: String = s"${attribute.toString} -> $operation $modification $value"

  def ++(other: Transformation): TransformationCollection = {
    TransformationCollection(this) ++ other
  }

  def ++(other: TransformationCollection): TransformationCollection = {
    TransformationCollection(this) ++ other
  }

  private def execute(
    action:          AttributeTransformation,
    playerAttribute: PlayerAttribute
  ): IO[Error, PlayerAttribute] = {
    if (playerAttribute.attribute === attribute) {
      IO.sync(
        playerAttribute.copy(
          value = action(playerAttribute.value)
        )
      )
    } else {
      IO.fail(
        Error(
          "Wrong attribute for transformation",
          s"Transformation apply to ${attribute.toString}, not for ${playerAttribute.toString}"
        )
      )
    }
  }

  def appliedTo(playerAttribute: PlayerAttribute): IO[Error, PlayerAttribute] = {
    execute(forwardTransformation, playerAttribute)
  }

  def revert(playerAttribute: PlayerAttribute): IO[Error, PlayerAttribute] = {
    execute(backwardTransformation, playerAttribute)
  }

  def canApply(playerAttribute: PlayerAttribute): Boolean = {
    playerAttribute.attribute === this.attribute
  }

  private def combineOperation(other: Transformation): Operation = {
    (this.operation, other.operation) match {
      case (a, b) if a == b => a
      case _                => Combination
    }
  }

  private def combineValue(other: Transformation): AttributeType = {
    (this.operation, other.operation) match {
      case (Addition, Addition) => this.value + other.value
      case (Multiply, Multiply) => this.value * other.value
      case _                    => 0
    }
  }

  override def |+|(other: AssemblyItemTrait): IO[Error, Transformation] = {
    other match {
      case t: Transformation if this.attribute === t.attribute =>
        val forward = (p: Int) => {
          p |> this.forwardTransformation |> t.forwardTransformation
        }
        val backward = (p: Int) => {
          p |> t.backwardTransformation |> this.backwardTransformation
        }

        IO.sync(
          Transformation(
            attribute = this.attribute,
            forwardTransformation = forward,
            backwardTransformation = backward,
            value = this combineValue t,
            operation = this combineOperation t,
            modification = if (forward(10) >= 10) {
              Increase
            } else {
              Decrease
            }
          )
        )
      case t: Transformation =>
        IO.fail(
          Error(
            "Cannot combine",
            s"Transformation applied to '${this.attribute.toString}' cannot combine " +
              s"with transformation applied to '${t.attribute.toString}'."
          )
        )
      case _ =>
        IO.fail(
          Error("Cannot combine", s"Cannot combine '${this.toString}' with '${other.toString}'")
        )
    }
  }
}

sealed trait Modification

final case object Increase extends Modification

final case object Decrease extends Modification

sealed trait Operation

final case object Addition extends Operation with FullOperation

final case object Multiply extends Operation with FullOperation

sealed trait FullOperation

final case object Reduce extends FullOperation

final case object Divide extends FullOperation

final case object Combination extends Operation

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
