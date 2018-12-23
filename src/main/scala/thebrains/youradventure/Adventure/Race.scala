package thebrains.youradventure.Adventure

import scalaz.zio.IO
import thebrains.youradventure.Adventure.AttributePack.{AttributeCollection, Attributes}
import thebrains.youradventure.Adventure.BodyPack.{BodyPart, BodyParts}
import thebrains.youradventure.Utils.Error

class Race(
  name:                String,
  description:         String,
  inputBaseAttributes: AttributeCollection,
  compoundAttributes:  Set[CompoundAttributes],
  inputBodyParts:      List[BodyPart]
) extends Things(name, description) {
  def bodyParts: List[BodyPart] = this.inputBodyParts

  def baseAttributes: AttributeCollection = inputBaseAttributes
}

object Races {
  case object Human
      extends Race(
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
          CompoundAttributes.Health
        )
      )

  val AllRaces: Seq[Race] = Seq(
    Human
  )

  lazy private val ListOfAllRaces: String = AllRaces.map(_.getName).mkString(", ")

  def fromString(race: String): IO[Error, Race] = {
    race.toLowerCase match {
      case Human.getLowerCaseName => IO.sync(Human)
      case _ =>
        IO.fail(
          Error(
            "Unknown Race",
            s"The race '$race' was not found among: $ListOfAllRaces"
          )
        )
    }
  }

}
