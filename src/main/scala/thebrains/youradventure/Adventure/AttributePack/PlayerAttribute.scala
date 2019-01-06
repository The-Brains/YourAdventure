package thebrains.youradventure.Adventure.AttributePack

import io.circe.{Encoder, Json}
import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Utils.{Err, ErrorIO}
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

  @transient lazy val getValue: PlayerAttribute.AttributeType = value

  override def encoded: Json = this.asJson

  override def toString: String = encoded.noSpaces

  override def |+|(other: AssemblyItemTrait): IO[Err, PlayerAttribute] = {
    other match {
      case p @ PlayerAttribute(a, v) =>
        if (this.attribute === a) {
          IO.sync(this.copy(value = this.getValue + v))
        } else {
          ErrorIO(
            "Impossible to merge",
            s"Impossible to merge '${this.toString}' with ${p.toString}"
          )
        }
      case _ =>
        ErrorIO(
          "Impossible merge",
          s"Impossible to merge '${this.toString}' with ${other.toString}"
        )
    }
  }

  def ++(other: PlayerAttribute): IO[Err, AttributeCollection] = {
    AttributeCollection(this) ++ AttributeCollection(other) match {
      case a: AttributeCollection => IO.sync(a)
      case _ =>
        ErrorIO(
          "Cannot convert",
          "Somehow, not able to combine two 'AttributeCollection' into one."
        )
    }
  }
}

object PlayerAttribute {
  type AttributeType = Int
  lazy val AttributeMinValue: AttributeType = Int.MinValue
  lazy val AttributeMaxValue: AttributeType = Int.MaxValue
  type AttributeTransformation = AttributeType => AttributeType
}
