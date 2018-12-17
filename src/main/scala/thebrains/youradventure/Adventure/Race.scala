package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.Attribute.{AttributeCollection, Attributes}

case class Race(
  name:               String,
  description:        String,
  baseAttributes:     AttributeCollection,
  compoundAttributes: Set[CompoundAttributes],
  bodyParts:          List[BodyPart]
) extends Things(name, description)

object Races {
  val Human: Race = Race(
    "Human",
    "Just a basic human",
    bodyParts = List(
      BodyParts.OneChest,
      BodyParts.OneHead
    ) ++
      BodyParts.TwoEars ++
      BodyParts.TwoArms ++
      BodyParts.TwoLegs ++
      BodyParts.TwoFeet ++
      BodyParts.TwoHands,
    baseAttributes = AttributeCollection(
      Attributes.Strength.toPlayerAttribute(10)
    ),
    compoundAttributes = Set(
      Attributes.Health
    )
  )
}
