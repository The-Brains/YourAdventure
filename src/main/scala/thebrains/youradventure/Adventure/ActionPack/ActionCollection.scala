package thebrains.youradventure.Adventure.ActionPack

import scalaz.Maybe
import scalaz.Maybe.Just
import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Utils.Error

import scala.util.Try

class ActionCollection(
  actions: List[Action],
  question: Maybe[String]
) extends BastardActionCollection(actions) {

  def getQuestion: Maybe[String] = question

  override protected def empty: ActionCollection = ActionCollection.Empty

  private def findAction(actionName: String): IO[Error, Action] = {
    getActions.find(a => a.getLowerCaseName == actionName.toLowerCase) match {
      case Some(a) => IO.sync(a)
      case None =>
        IO.fail(
          Error(
            "Action not found",
            s"Could not find action for '$actionName', among: ${validActions.mkString(", ")}"
          )
        )
    }
  }

  private def findAction(actionIndex: Int): IO[Error, Action] = {
    getIndexedActionsMap.get(actionIndex) match {
      case Some(a) => IO.sync(a)
      case None =>
        IO.fail(
          Error(
            "Action not found",
            s"Could not find action for id '$actionIndex', " +
              s"among: ${getIndexedActionsMap.keys.mkString(", ")}"
          )
        )
    }
  }

  def getAction(key: String): IO[Error, Action] = {
    key match {
      case i if Try(i.toInt).toOption.isDefined => findAction(i.toInt)
      case k if k.nonEmpty => findAction(k)
      case k: String if k.isEmpty =>
        IO.fail(Error("Empty input", "You have not entered anything"))
    }
  }
}

class BastardActionCollection(actions: List[Action])
  extends AssemblyTrait[BastardActionCollection, Action](actions) {

  def getActions: List[Action] = actions

  @transient lazy val getIndexedActions: List[(Int, Action)] = actions.zipWithIndex.map(_.swap)
  @transient lazy val getIndexedActionsMap: Map[Int, Action] = getIndexedActions.toMap

  @transient lazy val validActions: List[String] = getActions.map(_.getLowerCaseName)

  override protected def wrap(items: Action*): BastardActionCollection = {
    new BastardActionCollection(items.toList)
  }

  def ++(other: ActionCollection): ActionCollection = {
    new ActionCollection(actions = this.getActions ++ other.getActions, other.getQuestion)
  }

  override protected def empty: BastardActionCollection = BastardActionCollection.Empty
}

object BastardActionCollection {
  def apply(action: Action): BastardActionCollection = {
    new BastardActionCollection(List(action))
  }

  case object Empty extends BastardActionCollection(Nil)

}

object ActionCollection {
  def apply(
    action: Action,
    question: String
  ): ActionCollection = {
    new ActionCollection(List(action), Just(question))
  }

  def apply(question: String)(actions: Action*): ActionCollection = {
    new ActionCollection(actions.toList, Just(question))
  }

  case object Empty extends ActionCollection(Nil, Maybe.empty)

}
