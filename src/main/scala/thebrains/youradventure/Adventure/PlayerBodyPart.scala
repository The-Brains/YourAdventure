package thebrains.youradventure.Adventure

case class PlayerBodyPart(
  bodyPart:  BodyPart,
  equipment: Option[Equipment]
) {
  def canEquip(equipment: Equipment): Boolean = {
    bodyPart samePart equipment.bodyPart
  }

  def equip(equipment: Equipment): PlayerBodyPart = {
    assert(canEquip(equipment))
    this.copy(equipment = Some(equipment))
  }
}
