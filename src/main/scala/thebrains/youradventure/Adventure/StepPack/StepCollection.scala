package thebrains.youradventure.Adventure.StepPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Adventure.CollectionPack.ListImplicits._
import thebrains.youradventure.Adventure.StepPack.Step.StepName

class StepCollection(steps: Step*) extends AssemblyTrait[StepCollection, Step](steps.toList) {
  def getStep(stepName: StepName): Maybe[Step] = Maybe.fromOption(steps.find(_.getName == stepName))

  override protected def wrap(
    items: Step*
  ): StepCollection = {
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
