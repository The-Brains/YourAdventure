package thebrains.youradventure.Adventure.StepPack

import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Adventure.CollectionPack.ListImplicits._
import thebrains.youradventure.Adventure.StepPack.Step.StepName
import thebrains.youradventure.Utils.Error

class StepCollection(steps: List[Step]) extends AssemblyTrait[StepCollection, Step](steps) {
  def getStep(stepName: StepName): IO[Error, Step] = {
    steps.find(_.getName == stepName) match {
      case Some(step) => IO.sync(step)
      case None =>
        IO.fail(
          Error(
            "Cannot find step",
            s"Step with name '$stepName" +
              s"' could not be found amound: ${steps.map(_.getName).mkString(", ")} ."
          )
        )
    }
  }

  override protected def wrap(items: Step*): StepCollection = {
    StepCollection(items.toList)
  }

  def :+(item: Step): StepCollection = {
    this ++ item
  }

  def getExtras: List[Step] = getItems.getExtras

  override protected def empty: StepCollection = StepCollection.Empty
}

object StepCollection {
  val Empty: StepCollection = new StepCollection(Nil)

  def apply(steps: Step*): StepCollection = new StepCollection(steps.toList)

  def apply(steps: List[Step]): StepCollection = new StepCollection(steps)

}
