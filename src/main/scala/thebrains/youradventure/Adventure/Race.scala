package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.Attribute.{AttributeCollection, Attributes}
import thebrains.youradventure.Adventure.Body.{BodyPart, BodyParts}

class Race(
  name: String,
  description: String,
  inputBaseAttributes: AttributeCollection,
  compoundAttributes: Set[CompoundAttributes],
  inputBodyParts: List[BodyPart]
) extends Things(name, description) {
  def bodyParts: List[BodyPart] = this.inputBodyParts

  def baseAttributes: AttributeCollection = inputBaseAttributes
}

object Races {
  case object Human extends Race(
    name = "Human",
    description = "Just a basic human",
    inputBodyParts = List(
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
      Attributes.Health
    )
  )

  val AllRaces: Seq[Race] = Seq(
    Human
  )

  private lazy val ListOfAllRaces: String = AllRaces.map(_.getName).mkString(", ")

  def fromString(race: String): Either[Error, Race] = {
    race.toLowerCase match {
      case Human.getLowerCaseName => Right(Human)
      case _ => Left(Error(
        "Unknown Race",
        s"The race '$race' was not found among: $ListOfAllRaces"
      ))
    }
  }

}
