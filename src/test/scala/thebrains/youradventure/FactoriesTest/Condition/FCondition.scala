package thebrains.youradventure.FactoriesTest.Condition

import scalaz.Maybe
import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute.AttributeType
import thebrains.youradventure.Adventure.AttributePack.{Attribute, PlayerAttribute}
import thebrains.youradventure.Adventure.ConditionPack._
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FactoriesTest.AttributePack.FAttribute
import thebrains.youradventure.FactoriesTest.DefaultValues._
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine
import thebrains.youradventure.FactoriesTest.{FEquipment, FRace}
import thebrains.youradventure.Utils.ToOption._
import scala.util.Random

object FCondition {
  def apply(
    conditionLength: Maybe[Int] = Maybe.empty,
    conditions:      Maybe[List[ConditionOn]] = Maybe.empty
  )(
    implicit r: Random
  ): Conditions = {
    Conditions(
      conditions = conditions.getOrElse {
        (0 until (conditionLength getOrElse RandomMachine.getInt(0, DefaultListMaxLength))).map {
          _ =>
            RandomMachine.getFrom(
              List(
                FConditionOn.attribute(defaultWhenAttributeDoesNotExist = true),
                FConditionOn.equipment(),
                FConditionOn.race()
              )
            )
        }.toList
      }: _*
    )
  }
}

object FConditionOn {
  def attributeTrueFor(
    attribute: PlayerAttribute
  )(
    implicit r: Random
  ): ConditionTypes.AttributeCondition = {
    Conditions.create.forAttribute(
      attribute = attribute.getAttribute,
      minValue = attribute.getValue - RandomMachine.getInt(1, 10),
      maxValue = attribute.getValue + RandomMachine.getInt(1, 10),
      defaultWhenAttributeDoesNotExist = false
    )
  }

  def raceTrueFor(race: Race): ConditionTypes.RaceCondition = {
    Conditions.create.forRace(race)
  }

  def equipmentTrueFor(equipment: Equipment): ConditionTypes.EquipmentCondition = {
    Conditions.create.forEquipment(equipment)
  }

  def trueFor(p: Player)(implicit r: Random): ConditionOn = {
    RandomMachine.getFrom(
      List(
        attributeTrueFor(RandomMachine.getFrom(p.getBaseAttributes.getItems)).some,
        raceTrueFor(p.getRace).some,
        if (p.getEquipments.nonEmpty) {
          equipmentTrueFor(RandomMachine.getFrom(p.getEquipments)).some
        } else {
          None
        }
      ).flatten
    )
  }

  def attribute(
    attribute:                        Maybe[Attribute] = Maybe.empty,
    minValue:                         Maybe[AttributeType] = Maybe.empty,
    maxValue:                         Maybe[AttributeType] = Maybe.empty,
    defaultWhenAttributeDoesNotExist: Boolean
  )(
    implicit r: Random
  ): ConditionTypes.AttributeCondition = {
    val min = minValue getOrElse RandomMachine.getInt(DefaultMinAttribute, DefaultMaxAttribute)
    val max = maxValue getOrElse RandomMachine.getInt(DefaultMinAttribute, DefaultMaxAttribute)

    Conditions.create.forAttribute(
      attribute = attribute getOrElse FAttribute(),
      minValue = Math.min(min, max),
      maxValue = Math.max(min, max),
      defaultWhenAttributeDoesNotExist = defaultWhenAttributeDoesNotExist
    )
  }

  def equipment(
    equipment: Maybe[Equipment] = Maybe.empty
  )(
    implicit r: Random
  ): ConditionTypes.EquipmentCondition = {
    Conditions.create.forEquipment(
      equipment = equipment getOrElse FEquipment()
    )
  }

  def race(race: Maybe[Race] = Maybe.empty)(implicit r: Random): ConditionTypes.RaceCondition = {
    Conditions.create.forRace(
      equalTo = race getOrElse FRace()
    )
  }
}
