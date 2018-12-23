package thebrains.youradventure.Adventure.AttributePack

import io.circe.{Encoder, Json}
import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Utils.Error
import io.circe.generic.auto._
import io.circe.syntax._

case class PlayerAttribute(
  attribute: Attribute,
  value:     PlayerAttribute.AttributeType
) extends AssemblyItemTrait(
      attribute.getName,
      attribute.getDescription
    ) {
  implicit private val jsonEncoder: Encoder[PlayerAttribute] =
    Encoder.forProduct2[PlayerAttribute, String, PlayerAttribute.AttributeType]("name", "value") {
      case PlayerAttribute(a, v) => (a.getName, v)
    }

  override def encoded: Json = this.asJson

  override def toString: String = this.asJson.noSpaces

  def |+|(other: PlayerAttribute): Either[Error, PlayerAttribute] = {
    if (this === other) {
      Right(this.copy(value = this.value + other.value))
    } else {
      Left(
        Error(
          "Cannot combine attributes",
          s"Cannot combine attribute ${this.attribute.toString} with ${other.attribute.toString}"
        )
      )
    }
  }

  override def |+|(other: AssemblyItemTrait): IO[Error, PlayerAttribute] = {
    other match {
      case p: PlayerAttribute => this |+| p
      case _ =>
        IO.fail(
          Error(
            "Impossible merge",
            s"Impossible to merge '${this.toString}' with ${other.toString}"
          )
        )
    }
  }

  def ++(other: PlayerAttribute): IO[Error, AttributeCollection] = {
    AttributeCollection(this) ++ AttributeCollection(other) match {
      case a: AttributeCollection => IO.sync(a)
      case _ =>
        IO.fail(
          Error(
            "Cannot convert",
            "Somehow, not able to combine two 'AttributeCollection' into one."
          )
        )
    }
  }
}

object PlayerAttribute {
  type AttributeType = Int
  type AttributeTransformation = AttributeType => AttributeType
}
