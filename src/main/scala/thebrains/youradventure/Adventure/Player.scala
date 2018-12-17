package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.PlayerAttribute._
import thebrains.youradventure.Adventure.PlayerAttribute.AttributeCollection

case class Player(
  name:           String,
  journey:        List[Step],
  consumables:    List[Consumable],
  bodyParts:      List[PlayerBodyPart],
  baseAttributes: AttributeCollection,
  race:           Race
) {
  private def equipments: List[Equipment] = bodyParts.flatMap(_.equipment)

  private def equipmentModifier: List[Transformation] = equipments.flatMap(_.modifiers)

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
  }

  def create(name: String): PlayerWithName = {
    PlayerWithName(name)
  }
}
