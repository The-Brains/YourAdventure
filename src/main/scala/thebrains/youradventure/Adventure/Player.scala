package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.Attribute.AttributeCollection
import thebrains.youradventure.Adventure.Transformation.TransformationCollection

case class Player(
  name: String,
  journey: List[Step],
  consumables: List[Consumable],
  bodyParts: List[PlayerBodyPart],
  baseAttributes: AttributeCollection,
  race: Race
) {
  private def equipments: List[Equipment] = bodyParts.flatMap(_.equipment)

  private def equipmentModifier: TransformationCollection = {
    equipments
      .map(_.modifiers)
      .foldLeft[TransformationCollection](TransformationCollection.Empty)(_ ++ _)
  }

  def currentAttributes: AttributeCollection = baseAttributes << equipmentModifier
}

object PlayerBuilder {

  case class PlayerWithName(name: String) {
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

    def selectRace(race: String): Either[Error, Player] = {
      Races.fromString(race) match {
        case Right(r) => Right(selectRace(r))
        case Left(e) => Left(e)
      }
    }
  }

  def create(name: String): PlayerWithName = {
    PlayerWithName(name)
  }
}
