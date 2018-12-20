package thebrains.youradventure.Adventure.ActionPack

import scalaz.Maybe
import scalaz.Maybe.Just
import thebrains.youradventure.Utils.Error

import scala.util.Try

class ActionCollection(
  actions:  List[Action],
  question: Maybe[String]
) extends BastardActionCollection(actions) {

  def getQuestion: Maybe[String] = question

  override def ++(other: ActionCollection): ActionCollection = {
    ActionCollection.append(this, other)
  }

  override def ++(other: BastardActionCollection): ActionCollection = {
    ActionCollection.append(this, new ActionCollection(other.getActions, Maybe.empty))
  }

  override def ++(other: Action): ActionCollection = {
    this ++ other.asCollection
  }

  def getAction(key: String): Either[Error, Maybe[Action]] = {
    key match {
      case i if Try(i.toInt).toOption.isDefined =>
        getIndexedActionsMap.get(i.toInt) match {
          case Some(a) => Right(Just(a))
          case None =>
            Left(
              Error(
                "Action not found",
                s"Could not find action for id '$i', " +
                  s"among: ${getIndexedActionsMap.keys.mkString(", ")}"
              )
            )
        }
      case k =>
        getActions.find(a => a.getLowerCaseName == k.toLowerCase) match {
          case Some(a) => Right(Just(a))
          case None =>
            Left(
              Error(
                "Action not found",
                s"Could not find action for '$k', among: ${validActions.mkString(", ")}"
              )
            )
        }
    }
  }
}

class BastardActionCollection(actions: List[Action]) {
  def isEmpty: Boolean = actions.isEmpty

  def nonEmpty: Boolean = actions.nonEmpty

  def getActions: List[Action] = actions

  @transient lazy val getIndexedActions:    List[(Int, Action)] = actions.zipWithIndex.map(_.swap)
  @transient lazy val getIndexedActionsMap: Map[Int, Action] = getIndexedActions.toMap

  @transient lazy val validActions: List[String] = getActions.map(_.getLowerCaseName)

  def toCustomMap: Map[String, Action] = {
    actions.map(a => (a.getName, a)).toMap
  }

  def map(f: Action => Action): List[Action] = {
    actions.map(f)
  }

  def flatMap(f: Action => List[Action]): List[Action] = {
    actions.flatMap(f)
  }

  def filter(p: Action ⇒ Boolean): List[Action] = {
    actions.filter(p)
  }

  def foreach(f: Action => Unit): Unit = {
    actions.foreach(f)
  }

  def ++(other: BastardActionCollection): BastardActionCollection = {
    BastardActionCollection.append(this, other)
  }

  def ++(other: Action): BastardActionCollection = {
    BastardActionCollection.append(this, other.asCollection)
  }

  def ++(other: ActionCollection): ActionCollection = {
    ActionCollection.append(this, other)
  }
}

object BastardActionCollection extends scalaz.Monoid[BastardActionCollection] {
  def apply(action: Action): BastardActionCollection = {
    new BastardActionCollection(List(action))
  }

  case object Empty extends BastardActionCollection(Nil)

  override def zero: BastardActionCollection = Empty

  override def append(
    f1: BastardActionCollection,
    f2: => BastardActionCollection
  ): BastardActionCollection = {
    new BastardActionCollection(f1.getActions ++ f2.getActions)
  }
}

object ActionCollection extends scalaz.Monoid[ActionCollection] {
  def apply(
    action:   Action,
    question: String
  ): ActionCollection = {
    new ActionCollection(List(action), Just(question))
  }

  def apply(question: String)(actions: Action*): ActionCollection = {
    new ActionCollection(actions.toList, Just(question))
  }

  case object Empty extends ActionCollection(Nil, Maybe.empty)

  override def zero: ActionCollection = Empty

  override def append(
    f1: ActionCollection,
    f2: => ActionCollection
  ): ActionCollection = {
    new ActionCollection(f1.getActions ++ f2.getActions, f1.getQuestion orElse f2.getQuestion)
  }

  def append(
    f1: BastardActionCollection,
    f2: => ActionCollection
  ): ActionCollection = {
    new ActionCollection(f1.getActions ++ f2.getActions, f2.getQuestion)
  }
}
