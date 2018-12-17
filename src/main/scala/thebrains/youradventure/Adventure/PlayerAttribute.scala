package thebrains.youradventure.Adventure

case class PlayerAttribute(
  attribute: Attribute,
  value:     PlayerAttribute.AttributeType
) {
  def ===(other: PlayerAttribute): Boolean = {
    this.attribute === other
  }

  override def hashCode(): Int = attribute.name.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case p: PlayerAttribute => p === this
    case _ => false
  }
}

object PlayerAttribute {
  type AttributeType = Int
  type AttributeTransformation = AttributeType => AttributeType
  type AttributeCollection = Set[PlayerAttribute]

  implicit class PlayerAttributeCombine(attributes: AttributeCollection) {
    def |+|(other: AttributeCollection): AttributeCollection = {
      for {
        attribute1 <- attributes
        attribute2 <- other if attribute1 === attribute2
      } yield {
        attribute1.copy(value = attribute1.value + attribute2.value)
      }
    }

    def <<(transformations: List[Transformation]): AttributeCollection = {
      for {
        attribute      <- attributes
        transformation <- transformations if transformation canApply attribute
      } yield {
        transformation applyOn attribute
      }
    }
  }

}
