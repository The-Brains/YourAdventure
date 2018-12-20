package thebrains.youradventure.TerminalUIPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.ActionPack.Action
import thebrains.youradventure.Adventure._
import thebrains.youradventure.Utils

class GameStatus(
  universe:      Universe,
  currentStep:   Step,
  currentAction: Maybe[Action],
  player:        Maybe[Player]
) {

  private def copy(
    universe:      Maybe[Universe] = Maybe.empty,
    currentStep:   Maybe[Step] = Maybe.empty,
    currentAction: Maybe[Action] = Maybe.empty,
    player:        Maybe[Player] = Maybe.empty
  ): GameStatus = {
    new GameStatus(
      universe = universe.getOrElse(this.universe),
      currentStep = currentStep.getOrElse(this.currentStep),
      currentAction = currentAction.orElse(this.currentAction),
      player = player.orElse(this.player)
    )
  }

  def withPlayer(p: Either[Utils.Error, Player]): Either[Utils.Error, GameStatus] = {
    p.map(player => this.copy(player = Maybe.Just(player)))
  }

  private def selectRace(
    r:       Renderer,
    p:       PlayerBuilder.PlayerWithName,
    answers: List[Maybe[String]]
  ): Either[Utils.Error, Player] = {
    val firstAnswer: Maybe[String] = answers.headOption match {
      case Some(a) => a
      case None    => Maybe.empty
    }

    val race = r
      .display(p)
      .displayWithQuestionAnd(r.getTP, firstAnswer)
    p.selectRace(race) match {
      case Left(error) =>
        r.display(error).map { case (tp, message) => message.displayWith(tp) }
        if (!error.isFatal) {
          selectRace(r, p, answers.drop(1))
        } else {
          Left(error)
        }
      case Right(finalPlayer) => Right(finalPlayer)
    }
  }

  def startGame(
    r:       Renderer,
    answers: List[Maybe[String]] = Nil
  ): Either[Utils.Error, GameStatus] = {
    val firstAnswer: Maybe[String] = answers.headOption match {
      case Some(a) => a
      case None    => Maybe.empty
    }

    player match {
      case Maybe.Empty() =>
        val name = r.displayEmptyPlayer
          .displayWithQuestionAnd(r.getTP, firstAnswer)
        val p = PlayerBuilder.create(name)
        selectRace(r, p, answers.drop(1)) match {
          case Left(error) => Left(error)
          case Right(pl) =>
            Right(
              this.copy(
                player = Maybe.just(pl)
              )
            )
        }
      case Maybe.Just(_) => Right(this)
    }
  }

  def getPlayer: Maybe[Player] = this.player

  protected def getUniverse: Universe = this.universe

  protected def getCurrentAction: Maybe[Action] = this.currentAction

  protected def getCurrentStep: Step = this.currentStep
}

object GameStatus {

  def unapply(arg: GameStatus): Option[(Universe, Step, Maybe[Action], Maybe[Player])] = {
    Some(arg.getUniverse, arg.getCurrentStep, arg.getCurrentAction, arg.getPlayer)
  }

  def apply(universe: Universe): GameStatus = {
    new GameStatus(universe, universe.getStartingStep, Maybe.empty, Maybe.empty)
  }
}
