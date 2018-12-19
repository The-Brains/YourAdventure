package thebrains.youradventure.Adventure.BodyPack

import thebrains.youradventure.Adventure.{PlayerBodyPart, Things}

// TODO: Set a BodyPart collection like Attribute and Transformation

case class BodyPart(
  name:        String,
  description: String,
  descriptor:  String
) extends Things(name, description) {
  def toPlayerBodyPart: PlayerBodyPart = PlayerBodyPart(this, None)

  def samePart(other: BodyPart): Boolean = name == other.name

  def sameExactPart(other: BodyPart): Boolean = {
    samePart(other) &&
    this.descriptor == other.descriptor
  }

  override def toString: String = {
    if (descriptor.isEmpty) {
      super.toString
    } else {
      s"'$descriptor $name'"
    }
  }
}

// TODO: Issue for two hands items or several part for one item
object BodyParts {

  class PlainBodyPart(
    name:        String,
    description: String
  ) {
    def apply(descriptor: String): BodyPart = BodyPart(name, description, descriptor)
  }

  private def both(bodyPart: PlainBodyPart): List[BodyPart] = {
    List(bodyPart("left"), bodyPart("right"))
  }

  case object Leg
      extends PlainBodyPart(
        name = "leg",
        description = "This allow you to move"
      )

  val TwoLegs: List[BodyPart] = both(Leg)

  case object Arm
      extends PlainBodyPart(
        name = "arm",
        description = "This allow you to manage your hand"
      )

  val TwoArms: List[BodyPart] = both(Arm)

  case object Hand
      extends PlainBodyPart(
        name = "hand",
        description = "This allow you to grab things"
      )

  val TwoHands: List[BodyPart] = both(Hand)

  case object Foot
      extends PlainBodyPart(
        name = "foot",
        description = "This allow you to move on the ground"
      )

  val TwoFeet: List[BodyPart] = both(Foot)

  case object Head
      extends PlainBodyPart(
        name = "head",
        description = "This carry your senses."
      )

  val OneHead: BodyPart = Head("")

  case object Ear
      extends PlainBodyPart(
        name = "ear",
        description = "This allow you to hear or carry earrings."
      )

  val TwoEars: List[BodyPart] = both(Ear)

  case object Chest
      extends PlainBodyPart(
        name = "chest",
        description = "This is where all your limbs are connected to."
      )

  val OneChest: BodyPart = Chest("")
}
