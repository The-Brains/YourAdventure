package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.Transformation.TransformationCollection

case class Step(
  name:             String,
  description:      String,
  location:         Location,
  transformations:  TransformationCollection,
  availableActions: List[Action]
) extends Things(name, description) {}
