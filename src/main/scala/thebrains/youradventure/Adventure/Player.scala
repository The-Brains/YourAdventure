package thebrains.youradventure.Adventure

import scalaz.zio.IO
import thebrains.youradventure.Adventure.AttributePack.AttributeCollection
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.Utils.Error

case class Player(
  name: String,
  journey: List[Step],
  consumables: List[Consumable],
  bodyParts: List[PlayerBodyPart],
  baseAttributes: AttributeCollection,
  race: Race
) extends PlayerTrait {
  private def equipments: List[Equipment] = bodyParts.flatMap(_.equipment)

  private def equipmentModifier: TransformationCollection = {
    equipments
      .map(_.modifiers)
      .foldLeft[TransformationCollection](TransformationCollection.Empty)(_ ++ _)
  }

  def currentAttributes: IO[Error, AttributeCollection] = baseAttributes << equipmentModifier

  def toStatus: String = ""

  def addHistory(s: Step): IO[Nothing, Player] = {
    IO.sync(this.copy(journey = journey :+ s))
  }
}

sealed trait PlayerTrait

object PlayerBuilder {
  val NameQuestion: String = "What is your name?"
  val RaceQuestion: String = "What race are you part of ?"

  case class PlayerWithName(name: String) extends PlayerTrait {
    def selectRace(race: Race): Player = {
      Player(
        name = name,
        journey = Nil,
        consumables = Nil,
        bodyParts = race.bodyParts.map(_.toPlayerBodyPart),
        baseAttributes = race.baseAttributes,
        race = race
      )
    }

    def selectRace(availableRaces: List[Race])(race: String): IO[Error, Player] = {
      availableRaces.find(_.getLowerCaseName == race.toLowerCase) match {
        case Some(r) => IO.sync(selectRace(r))
        case None => IO.fail(Error("Not found race",
          s"Could not found '$race' among: " +
            s"${availableRaces.map(_.getCapitalizeName).mkString(", ")}"))
      }
    }
  }

  def create(name: String): IO[Nothing, PlayerWithName] = {
    IO.sync(PlayerWithName(name))
  }
}
