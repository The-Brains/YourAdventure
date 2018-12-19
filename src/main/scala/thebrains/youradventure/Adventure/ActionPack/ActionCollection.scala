package thebrains.youradventure.Adventure.ActionPack

import scalaz.Maybe
import scalaz.Maybe.Just

class ActionCollection(
  actions:  List[Action],
  question: Maybe[String]
) extends BastardActionCollection(actions) {

  def getQuestion: Maybe[String] = question

  def ++(other: ActionCollection): ActionCollection = {
    ActionCollection.append(this, other)
  }

  override def ++(other: BastardActionCollection): ActionCollection = {
    ActionCollection.append(this, new ActionCollection(other.getActions, Maybe.empty))
  }

  override def ++(other: Action): ActionCollection = {
    this ++ other.asCollection
  }
}

class BastardActionCollection(actions: List[Action]) {
  def getActions: List[Action] = actions

  @transient lazy val getIndexedActions: List[(Int, Action)] = actions.zipWithIndex.map(_.swap)

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
}