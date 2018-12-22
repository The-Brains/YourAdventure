package thebrains.youradventure.Game

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.ActionPack.Action
import thebrains.youradventure.Adventure._
import thebrains.youradventure.FPTerminalIO._
import thebrains.youradventure.Utils.Error

class GameStatus(
  universe:      Universe,
  currentStep:   Step,
  currentAction: Maybe[Action],
  player:        Maybe[PlayerTrait],
  renderer:      Renderer,
  currentError:  Maybe[Error]
) {
  override def toString: String = {
    s"State(" +
      s"step:${currentStep.toString}, " +
      s"action:${currentAction.toString}, " +
      s"player:${player.toString}, " +
      s"error:${currentError.toString}" +
      s")"
  }

  def consume(input: Input): IO[Error, GameStatus] = consumer.consume(input)

  def getNextMessage: IO[Error, TerminalMessage] = producer.getNextMessage

  @transient lazy val updater: Updater = new Updater(this)

  @transient lazy val consumer: Consumer = new Consumer(this, updater)

  @transient lazy val producer: Producer = new Producer(this)

  @transient lazy val getPlayer: Maybe[PlayerTrait] = this.player

  @transient lazy val getUniverse: Universe = this.universe

  @transient lazy val getCurrentAction: Maybe[Action] = this.currentAction

  @transient lazy val getCurrentStep: Step = this.currentStep

  @transient lazy val getRenderer: Renderer = this.renderer

  @transient lazy val getError: Maybe[Error] = this.currentError

  @transient lazy val getCurrentError: Maybe[Error] = this.currentError
}

object GameStatus {

  def unapply(
    arg: GameStatus
  ): Option[(Universe, Maybe[Error], Step, Maybe[Action], Maybe[PlayerTrait], Renderer)] = {
    Some(
      arg.getUniverse,
      arg.getError,
      arg.getCurrentStep,
      arg.getCurrentAction,
      arg.getPlayer,
      arg.getRenderer
    )
  }

  def apply(universe: Universe): GameStatus = {
    new GameStatus(
      universe,
      universe.getStartingStep,
      Maybe.empty,
      Maybe.empty,
      Renderer(),
      Maybe.empty
    )
  }
}
