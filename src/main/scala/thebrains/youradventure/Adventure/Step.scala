package thebrains.youradventure.Adventure

case class Step(
  name:             String,
  description:      String,
  location:         Location,
  transformations:  List[Transformation],
  availableActions: List[Action]
) extends Things(name, description) {}
