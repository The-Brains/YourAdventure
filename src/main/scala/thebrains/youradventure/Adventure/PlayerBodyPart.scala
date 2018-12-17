package thebrains.youradventure.Adventure

case class PlayerBodyPart(
  bodyPart:  BodyPart,
  equipment: Option[Equipment]
) {
  def canEquip(equipment: Equipment): Boolean = {
    bodyPart samePart equipment.bodyPart
  }

  def equip(equipment: Equipment): Either[Error, PlayerBodyPart] = {
    if (canEquip(equipment)) {
      Right(this.copy(equipment = Some(equipment)))
    } else {
      Left(
        Error(
          "Wrong emplacement for equipment",
          s"The equipment ${equipment.toString} cannot be equipped on " +
            s"emplacement ${this.bodyPart.toString}"
        )
      )
    }

  }
}
