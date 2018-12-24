package thebrains.youradventure.Game

import io.circe.{Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._
import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack.Action
import thebrains.youradventure.Adventure.StepPack.Step
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FPTerminalIO._
import thebrains.youradventure.Utils.Error

class GameStatus(
  universe:      Universe,
  currentStep:   Maybe[Step],
  currentAction: Maybe[Action],
  player:        Maybe[PlayerTrait],
  renderer:      Renderer,
  currentError:  Maybe[Error]
) {

  implicit private val jsonEncoder: Encoder[GameStatus] =
    Encoder.forProduct4[GameStatus, Maybe[Json], Maybe[Json], Maybe[Json], Maybe[Json]](
      "step",
      "action",
      "player",
      "error"
    ) {
      case GameStatus(_, e, s, a, p, _) =>
        (s.map(_.encoded), a.map(_.encoded), p.map(_.encoded), e.map(_.encoded))
    }

  override def toString: String = this.asJson.noSpaces

  def consume(input: Input): IO[Error, GameStatus] = consumer.consume(input)

  def getNextMessage: IO[Error, TerminalMessage] = producer.getNextMessage

  @transient lazy val updater: Updater = new Updater(this)

  @transient lazy val consumer: Consumer = new Consumer(this, updater)

  @transient lazy val producer: Producer = new Producer(this)

  @transient lazy val getPlayer: Maybe[PlayerTrait] = this.player

  @transient lazy val getUniverse: Universe = this.universe

  @transient lazy val getCurrentAction: Maybe[Action] = this.currentAction

  @transient lazy val getCurrentStep: Maybe[Step] = this.currentStep

  @transient lazy val getRenderer: Renderer = this.renderer

  @transient lazy val getError: Maybe[Error] = this.currentError

  @transient lazy val getCurrentError: Maybe[Error] = this.currentError

  @transient lazy val isCompleted: Boolean = this.currentStep.isEmpty
}

object GameStatus {

  def unapply(
    arg: GameStatus
  ): Option[(Universe, Maybe[Error], Maybe[Step], Maybe[Action], Maybe[PlayerTrait], Renderer)] = {
    Some(
      arg.getUniverse,
      arg.getError,
      arg.getCurrentStep,
      arg.getCurrentAction,
      arg.getPlayer,
      arg.getRenderer
    )
  }

  def apply(
    renderer: Renderer,
    universe: Universe
  ): GameStatus = {
    new GameStatus(
      universe,
      Maybe.just(universe.getStartingStep),
      Maybe.empty,
      Maybe.empty,
      renderer,
      Maybe.empty
    )
  }
}
