package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.Transformation.Transformation

case class Step(
  name:             String,
  description:      String,
  location:         Location,
  transformations:  List[Transformation],
  availableActions: List[Action]
) extends Things(name, description) {}
