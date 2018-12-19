package thebrains.youradventure.TerminalUIPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.ActionPack.Action
import thebrains.youradventure.Adventure._

class GameStatus(
  universe: Universe,
  currentStep: Step,
  currentAction: Maybe[Action],
  player: Maybe[Player]
) {

  private def copy(
    universe: Maybe[Universe] = Maybe.empty,
    currentStep: Maybe[Step] = Maybe.empty,
    currentAction: Maybe[Action] = Maybe.empty,
    player: Maybe[Player] = Maybe.empty
  ): GameStatus = {
    new GameStatus(
      universe = universe.getOrElse(this.universe),
      currentStep = currentStep.getOrElse(this.currentStep),
      currentAction = currentAction.orElse(this.currentAction),
      player = player.orElse(this.player)
    )
  }

  private def selectRace(
    r: Renderer,
    p: PlayerBuilder.PlayerWithName,
    answers: List[Maybe[String]]
  ): Either[Error, Player] = {
    val firstAnswer: Maybe[String] = answers.headOption match {
      case Some(a) => a
      case None => Maybe.empty
    }

    val race = r.display(p)
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

  def startGame(r: Renderer, answers: List[Maybe[String]] = Nil): Either[Error, GameStatus] = {
    val firstAnswer: Maybe[String] = answers.headOption match {
      case Some(a) => a
      case None => Maybe.empty
    }

    player match {
      case Maybe.Empty() =>
        val name = r.displayEmptyPlayer
          .displayWithQuestionAnd(r.getTP, firstAnswer)
        val p = PlayerBuilder.create(name)
        selectRace(r, p, answers.drop(1)) match {
          case Left(error) => Left(error)
          case Right(pl) => Right(this.copy(
            player = Maybe.just(pl)
          ))
        }
      case Maybe.Just(_) => Right(this)
    }
  }

  def getPlayer: Maybe[Player] = this.player
}

object GameStatus {
  def apply(
    universe: Universe
  ): GameStatus = {
    new GameStatus(universe, universe.getStartingStep, Maybe.empty, Maybe.empty)
  }
}
