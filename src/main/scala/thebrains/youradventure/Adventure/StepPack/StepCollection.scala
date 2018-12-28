package thebrains.youradventure.Adventure.StepPack

import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Adventure.CollectionPack.ListImplicits._
import thebrains.youradventure.Adventure.StepPack.Step.StepName
import thebrains.youradventure.Utils.Error

class StepCollection(steps: Step*) extends AssemblyTrait[StepCollection, Step](steps.toList) {
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
}

object StepCollection {
  val Empty: StepCollection = new StepCollection()

  def apply(steps: Step*): StepCollection = new StepCollection(steps: _*)

  def apply(steps: List[Step]): StepCollection = new StepCollection(steps: _*)

}
