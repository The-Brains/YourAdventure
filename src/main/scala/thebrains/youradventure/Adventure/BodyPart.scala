package thebrains.youradventure.Adventure

case class BodyPart(
  name: String,
  description: String,
  descriptor: String
) extends Things(name, description) {
  def toPlayerBodyPart: PlayerBodyPart = PlayerBodyPart(this, None)

  def samePart(other: BodyPart): Boolean = name == other.name

  def sameExactPart(other: BodyPart): Boolean = {
    samePart(other) &&
      this.descriptor == other.descriptor
  }
}

object BodyParts {

  class PlainBodyPart(
    name: String,
    description: String
  ) {
    def apply(descriptor: String): BodyPart = BodyPart(name, description, descriptor)
  }

  private def both(bodyPart: PlainBodyPart): List[BodyPart] = {
    List(bodyPart("left"), bodyPart("right"))
  }

  val Leg: PlainBodyPart = new PlainBodyPart(
    "leg",
    "This allow you to move"
  )

  val TwoLegs: List[BodyPart] = both(Leg)

  val Arm: PlainBodyPart = new PlainBodyPart(
    "arm",
    "This allow you to manage your hand"
  )

  val TwoArms: List[BodyPart] = both(Arm)

  val Hand: PlainBodyPart = new PlainBodyPart(
    "hand",
    "This allow you to grab things"
  )

  val TwoHands: List[BodyPart] = both(Hand)

  val Foot: PlainBodyPart = new PlainBodyPart(
    "foot",
    "This allow you to move on the ground"
  )

  val TwoFeet: List[BodyPart] = both(Foot)

  val Head: PlainBodyPart = new PlainBodyPart(
    "head",
    "This carry your senses."
  )

  val OneHead: BodyPart = Head("")

  val Ear: PlainBodyPart = new PlainBodyPart(
    "ear",
    "This allow you to hear or carry earrings."
  )

  val TwoEars: List[BodyPart] = both(Ear)

  val Chest: PlainBodyPart = new PlainBodyPart(
    "chest",
    "This is where all your limbs are connected to."
  )

  val OneChest: BodyPart = Chest("")
}
