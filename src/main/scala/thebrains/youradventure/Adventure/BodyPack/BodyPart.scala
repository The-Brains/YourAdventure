package thebrains.youradventure.Adventure.BodyPack

import io.circe.syntax._
import io.circe.{Encoder, Json}
import scalaz.Maybe
import thebrains.youradventure.Adventure.CollectionPack.AssemblyItemTrait
import thebrains.youradventure.Utils.ToOption._

case class BodyPart(
  name:        String,
  description: String,
  descriptor:  Maybe[String]
) extends AssemblyItemTrait(name, description) {
  @transient lazy val toPlayerBodyPart: PlayerBodyPart = new PlayerBodyPart(this)
  @transient lazy val getDescriptor:    Maybe[String] = descriptor

  def samePartAs(other: BodyPart): Boolean = name == other.name

  def sameExactPartAs(other: BodyPart): Boolean = {
    samePartAs(other) &&
    this.descriptor == other.descriptor
  }

  implicit private val jsonEncoder: Encoder[BodyPart] =
    Encoder.forProduct2[BodyPart, String, String]("name", "descriptor") {
      case BodyPart(n, _, d) => (n, d.getOrElse(""))
    }

  override def encoded: Json = this.asJson

  override def toString: String = encoded.noSpaces
}

object BodyPart {
  implicit def ordering[A <: BodyPart]: Ordering[A] = {
    Ordering.by(a => (a.getName, a.getDescriptor.getOrElse("")))
  }
}

// TODO: Issue for two hands items or several part for one item
object BodyParts {

  class PlainBodyPart(
    name:        String,
    description: String
  ) {
    def apply(descriptor: String): BodyPart = BodyPart(name, description, descriptor.just)
  }

  private def both(bodyPart: PlainBodyPart): BodyCollection = {
    BodyCollection(bodyPart("left"), bodyPart("right"))
  }

  final case object Leg
      extends PlainBodyPart(
        name = "leg",
        description = "This allow you to move"
      )

  val TwoLegs: BodyCollection = both(Leg)

  final case object Arm
      extends PlainBodyPart(
        name = "arm",
        description = "This allow you to manage your hand"
      )

  val TwoArms: BodyCollection = both(Arm)

  final case object Hand
      extends PlainBodyPart(
        name = "hand",
        description = "This allow you to grab things"
      )

  val TwoHands: BodyCollection = both(Hand)

  final case object Foot
      extends PlainBodyPart(
        name = "foot",
        description = "This allow you to move on the ground"
      )

  val TwoFeet: BodyCollection = both(Foot)

  final case object Head
      extends PlainBodyPart(
        name = "head",
        description = "This carry your senses."
      )

  val OneHead: BodyPart = Head("")

  final case object Ear
      extends PlainBodyPart(
        name = "ear",
        description = "This allow you to hear or carry earrings."
      )

  val TwoEars: BodyCollection = both(Ear)

  final case object Chest
      extends PlainBodyPart(
        name = "chest",
        description = "This is where all your limbs are connected to."
      )

  val OneChest: BodyPart = Chest("")
}
