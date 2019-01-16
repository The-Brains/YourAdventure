package thebrains.youradventure.Adventure.TransformationPack

import io.circe.syntax._
import io.circe.{Encoder, Json}
import scalaz.zio.IO
import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute._
import thebrains.youradventure.Adventure.AttributePack._
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Utils.{Err, ErrorIO}

case class Transformation(
  attribute:     Attribute,
  value:         AttributeType,
  fullOperation: FullOperation
) extends AssemblyItemTrait(
      attribute.getName,
      attribute.getDescription
    ) {
  implicit private val jsonEncoder: Encoder[Transformation] =
    Encoder
      .forProduct3[Transformation, Json, AttributeType, String]("attribute", "value", "operation") {
        case Transformation(a, v, o) => (a.encoded, v, o.toString)
      }

  override def encoded: Json = this.asJson

  override def toString: String = encoded.noSpaces

  @transient lazy val asCollection = TransformationCollection(this)
  @transient lazy val modify: Modification = fullOperation match {
    case Addition | Multiply => Increase
    case Reduce | Divide     => Decrease
  }
  @transient lazy val operation: Operation = fullOperation match {
    case Addition | Reduce => Addition
    case Multiply | Divide => Multiply
  }
  @transient lazy val forwardTransformation: AttributeType => AttributeType = { a: AttributeType =>
    fullOperation match {
      case Addition => a + this.value
      case Reduce   => a - this.value
      case Multiply => a * this.value
      case Divide   => a / this.value
    }
  }
  @transient lazy val backwardTransformation: AttributeType => AttributeType = { a: AttributeType =>
    fullOperation match {
      case Addition => a - this.value
      case Reduce   => a + this.value
      case Multiply => a / this.value
      case Divide   => a * this.value
    }
  }

  def ++(other: Transformation): TransformationCollection = {
    TransformationCollection(this) ++ other
  }

  private def execute(
    action:          AttributeTransformation,
    playerAttribute: PlayerAttribute
  ): IO[Err, PlayerAttribute] = {
    if (this.canApply(playerAttribute)) {
      IO.sync(
        playerAttribute.copy(
          value = action(playerAttribute.value)
        )
      )
    } else {
      ErrorIO(
        "Wrong attribute for transformation",
        s"Transformation apply to ${attribute.toString}, not for ${playerAttribute.toString}"
      )
    }
  }

  private def safeExecute(
    action:          AttributeTransformation,
    playerAttribute: PlayerAttribute
  ): PlayerAttribute = {
    if (this.canApply(playerAttribute)) {
      playerAttribute.copy(value = action(playerAttribute.value))
    } else {
      playerAttribute
    }
  }

  def safeAppliedTo(playerAttribute: PlayerAttribute): PlayerAttribute = {
    safeExecute(forwardTransformation, playerAttribute)
  }

  def appliedTo(playerAttribute: PlayerAttribute): IO[Err, PlayerAttribute] = {
    execute(forwardTransformation, playerAttribute)
  }

  def revert(playerAttribute: PlayerAttribute): IO[Err, PlayerAttribute] = {
    execute(backwardTransformation, playerAttribute)
  }

  def safeRevert(playerAttribute: PlayerAttribute): PlayerAttribute = {
    safeExecute(backwardTransformation, playerAttribute)
  }

  def canApply(playerAttribute: PlayerAttribute): Boolean = {
    playerAttribute.attribute === this.attribute
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

object TransformationBuilder {

  @transient lazy val AvailableOperations: List[FullOperation] = List(
    Addition,
    Reduce,
    Multiply,
    Divide
  )

  def willDo(operation: FullOperation): TransformValue = {
    TransformValue(operation)
  }

  case class TransformValue private (fullOperation: FullOperation) {
    def byValueOf(value: AttributeType): TransformationWithValue = {
      TransformationWithValue(
        value,
        fullOperation
      )
    }
  }

  case class TransformationWithValue(
    value:         AttributeType,
    fullOperation: FullOperation
  ) {
    def onAttribute(attribute: Attribute): Transformation = {
      Transformation(
        attribute = attribute,
        value = value,
        fullOperation = fullOperation
      )
    }
  }
}
