package thebrains.youradventure.FactoriesTest.ActionPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.ActionPack.{Action, ActionCollection}
import thebrains.youradventure.FactoriesTest.Utils.RandomMachine

import scala.util.Random
import thebrains.youradventure.FactoriesTest.DefaultValues._

object FActionCollection {
  def apply(
    question:     Maybe[String] = Maybe.empty,
    lengthAction: Maybe[Int] = Maybe.empty,
    actions:      Maybe[List[Action]] = Maybe.empty
  )(
    implicit r: Random
  ): ActionCollection = {
    ActionCollection(
      question = question getOrElse RandomMachine.getString(DefaultQuestionLength)
    )(
      actions = actions.getOrElse(
        (0 until (lengthAction getOrElse RandomMachine.getInt(1, DefaultListMaxLength)))
          .map(_ => FAction())
          .toList
      ): _*
    )
  }
}
