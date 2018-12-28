package thebrains.youradventure.Adventure.ActionPack

import scalaz.Maybe
import scalaz.Maybe.Just
import scalaz.zio.IO
import thebrains.youradventure.Adventure.CollectionPack.AssemblyTrait
import thebrains.youradventure.Utils.Error

import scala.util.Try

class ActionCollection(
  actions:  List[Action],
  question: Maybe[String]
) extends BastardActionCollection(actions) {

  def getQuestion: Maybe[String] = question

  def getAction(key: String): IO[Error, Action] = {
    key match {
      case i if Try(i.toInt).toOption.isDefined =>
        getIndexedActionsMap.get(i.toInt) match {
          case Some(a) => IO.sync(a)
          case None =>
            IO.fail(
              Error(
                "Action not found",
                s"Could not find action for id '$i', " +
                  s"among: ${getIndexedActionsMap.keys.mkString(", ")}"
              )
            )
        }
      case k =>
        getActions.find(a => a.getLowerCaseName == k.toLowerCase) match {
          case Some(a) => IO.sync(a)
          case None =>
            IO.fail(
              Error(
                "Action not found",
                s"Could not find action for '$k', among: ${validActions.mkString(", ")}"
              )
            )
        }
    }
  }
}

class BastardActionCollection(actions: List[Action])
    extends AssemblyTrait[BastardActionCollection, Action](actions) {

  def getActions: List[Action] = actions

  @transient lazy val getIndexedActions:    List[(Int, Action)] = actions.zipWithIndex.map(_.swap)
  @transient lazy val getIndexedActionsMap: Map[Int, Action] = getIndexedActions.toMap

  @transient lazy val validActions: List[String] = getActions.map(_.getLowerCaseName)

  override protected def wrap(items: Action*): BastardActionCollection = {
    new BastardActionCollection(items.toList)
  }
}

object BastardActionCollection {
  def apply(action: Action): BastardActionCollection = {
    new BastardActionCollection(List(action))
  }

  case object Empty extends BastardActionCollection(Nil)

}

object ActionCollection {
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

}
