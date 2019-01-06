package thebrains.youradventure.Adventure

import scalaz.zio.IO
import thebrains.youradventure.Adventure.AttributePack.{AttributeCollection, Attributes}
import thebrains.youradventure.Adventure.BodyPack._
import thebrains.youradventure.Utils.{Err, ErrorIO}

class Race(
  name:                String,
  description:         String,
  inputBaseAttributes: AttributeCollection,
  compoundAttributes:  Set[CompoundAttributes],
  inputBodyParts:      BodyCollection
) extends Things(name, description) {
  @transient lazy val bodyParts: BodyCollection = this.inputBodyParts

  def baseAttributes: AttributeCollection = inputBaseAttributes
}

object Races {

  final case object Human
      extends Race(
        name = "Human",
        description = "Just a basic human",
        inputBodyParts = BodyCollection(
          BodyParts.OneChest,
          BodyParts.OneHead
        ) ++
          BodyParts.TwoEars ++
          BodyParts.TwoArms ++
          BodyParts.TwoLegs ++
          BodyParts.TwoFeet ++
          BodyParts.TwoHands,
        inputBaseAttributes = AttributeCollection(
          Attributes.Strength.toPlayerAttribute(10)
        ),
        compoundAttributes = Set(
          CompoundAttributes.Health
        )
      )

  val AllRaces: Seq[Race] = Seq(
    Human
  )

  lazy private val ListOfAllRaces: String = AllRaces.map(_.getName).mkString(", ")

  def fromString(race: String): IO[Err, Race] = {
    race.toLowerCase match {
      case Human.getLowerCaseName => IO.sync(Human)
      case _ =>
        ErrorIO(
          "Unknown Race",
          s"The race '$race' was not found among: $ListOfAllRaces"
        )
    }
  }

  final case object Void
      extends Race(
        name = "Empty Object",
        description = "Not a Race",
        inputBodyParts = BodyCollection.Empty,
        inputBaseAttributes = AttributeCollection.Empty,
        compoundAttributes = Set.empty
      )

}
