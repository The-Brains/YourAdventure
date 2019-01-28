package thebrains.youradventure.FactoriesTest

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.StepPack.{Step, StepCollection}
import thebrains.youradventure.Adventure.{Race, Universe}
import thebrains.youradventure.FactoriesTest.StepPack.FStep
import thebrains.youradventure.Utils.Err

import scala.util.Random

object FUniverse {
  def apply(
    lengthRace:   Maybe[Int] = Maybe.empty,
    races:        Maybe[List[Race]] = Maybe.empty,
    lengthStep:   Maybe[Int] = Maybe.empty,
    steps:        Maybe[List[Step]] = Maybe.empty,
    startingStep: Maybe[Step] = Maybe.empty
  )(
    implicit r: Random
  ): IO[Err, Universe] = {
    Universe.apply(
      availableRaces = RawFactory.getList(lengthRace, races, _ => FRace()),
      availableSteps = StepCollection(RawFactory.getList(lengthStep, steps, _ => FStep())),
      startingStep = startingStep getOrElse FStep()
    )
  }
}
