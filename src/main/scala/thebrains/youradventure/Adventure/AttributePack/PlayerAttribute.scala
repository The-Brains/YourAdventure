package thebrains.youradventure.Adventure.AttributePack

import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Utils.Error

case class PlayerAttribute(
  attribute: Attribute,
  value: PlayerAttribute.AttributeType
) extends AssemblyItemTrait(
  attribute.getName,
  attribute.getDescription
) {

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

  override def |+|(
    other: AssemblyItemTrait
  ): Either[Error, PlayerAttribute] = {
    other match {
      case p: PlayerAttribute => this |+| p
      case _ => Left(Error(
        "Impossible merge",
        s"Impossible to merge '${this.toString}' with ${other.toString}"
      ))
    }
  }

  def ++(other: PlayerAttribute): Either[Error, AttributeCollection] = {
    AttributeCollection(this) ++ AttributeCollection(other) match {
      case a: AttributeCollection => Right(a)
      case _ => Left(Error("Cannot convert",
        "Somehow, not able to combine two 'AttributeCollection' into one."))
    }
  }
}

object PlayerAttribute {
  type AttributeType = Int
  type AttributeTransformation = AttributeType => AttributeType
}
