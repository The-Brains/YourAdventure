package thebrains.youradventure.Adventure.ConditionPack

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.AttributePack.Attribute
import thebrains.youradventure.Adventure.AttributePack.PlayerAttribute._
import thebrains.youradventure.Adventure.CollectionPack.{AssemblyItemTrait, AssemblyTrait}
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils._

abstract class ConditionOn(
  name:        String,
  description: String
) extends AssemblyItemTrait(name, description) {
  def isTrueFor(p: Player): IO[Err, Boolean]
}

case class RaceCondition(equalTo: Race)
    extends ConditionOn(
      name = "Condition on race",
      description = s"Condition to be part of '${equalTo.getCapitalizeName}'."
    ) {
  override def isTrueFor(p: Player): IO[Err, Boolean] = IO.sync(p.getRace === equalTo)
}

case class AttributeCondition(
  attribute:                        Attribute,
  minValue:                         AttributeType = AttributeMinValue,
  maxValue:                         AttributeType = AttributeMaxValue,
  defaultWhenAttributeDoesNotExist: Boolean
) extends ConditionOn(
      name = "Condition on Attribute",
      description = s"Condition for attribute '${attribute.getCapitalizeName}' " +
        s"to be between $minValue and $maxValue."
    ) {
  override def isTrueFor(p: Player): IO[Err, Boolean] = {
    for {
      attributes <- p.currentAttributes
    } yield {
      attributes.getAttribute(this.attribute) match {
        case Maybe.Just(playerAttribute) =>
          playerAttribute.getValue <= maxValue && playerAttribute.getValue >= minValue
        case Maybe.Empty() =>
          defaultWhenAttributeDoesNotExist
      }
    }
  }
}

case class EquipmentCondition(equipment: Equipment)
    extends ConditionOn(
      name = "Condition on Equipment",
      description = s"Condition to wear equipment '${equipment.getCapitalizeName}'."
    ) {
  override def isTrueFor(p: Player): IO[Err, Boolean] = IO.sync(p isWearing equipment)
}

class Condition(
  conditions: List[ConditionOn]
  //  race: ConditionOn[Race],
  //  attribute: ConditionOn[PlayerAttribute],
  //  equipment: ConditionOn[Equipment],
  //  step: ConditionOn[Step],
  //  consumable: ConditionOn[Consumable],
  //  bodyPart: ConditionOn[BodyPart]
) extends AssemblyTrait[Condition, ConditionOn](conditions) {
  def isTrueFor(p: Player): IO[Err, Boolean] = {
    IO.sequence(conditions.map(_ isTrueFor p)).map(_.reduce(_ && _))
  }

  override protected def wrap(items: ConditionOn*): Condition = {
    new Condition(items.toList)
  }

  override protected def empty: Condition = Condition.Empty
}

object Condition {
  def apply(conditions: ConditionOn*): Condition = {
    new Condition(conditions.toList)
  }

  lazy val Empty: Condition = new Condition(Nil)
}
