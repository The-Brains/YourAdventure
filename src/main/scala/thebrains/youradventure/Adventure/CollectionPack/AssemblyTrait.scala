package thebrains.youradventure.Adventure.CollectionPack

import io.circe.Json
import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Utils.Error
import ListImplicits._
import scala.reflect.ClassTag

abstract class AssemblyTrait[THIS <: AssemblyTrait[THIS, A], A <: AssemblyItemTrait : ClassTag](
  items: List[A]
) {
  def toCustomMap: Map[String, A] = {
    items.map(a => (a.getName, a)).toMap
  }

  def encoded: List[Json] = items.map(_.encoded)

  override def toString: String = s"[${items.map(_.toString).mkString(", ")}]"

  def isEmpty: Boolean = items.isEmpty

  def nonEmpty: Boolean = items.nonEmpty

  def foreach(f: A => Unit): Unit = items.foreach(f)

  protected def reduceAll: IO[Error, A] = {
    items match {
      case head :: second :: tail =>
        val c = head |+| second
        c.flatMap {
          case a: A => wrap(a +: tail: _*).reduceAll
          case _ => IO.fail(Error("Cannot convert", "Cannot convert to 'A'."))
        }
      case head :: second :: Nil =>
        (head |+| second).flatMap {
          case a: A => IO.sync(a)
          case _ => IO.fail(Error("Cannot convert", "Cannot convert to 'A'."))
        }
      case head :: Nil => IO.sync(head)
      case Nil => IO.fail(Error("Empty list", "There is nothing to combine"))
    }
  }

  def find(p: A => Boolean): Maybe[A] = Maybe.fromOption(items.find(p))

  def map(f: A => A): THIS = wrap(items.map(f): _*)

  def outMap[B](f: A => B): List[B] = items.map(f)

  def flatMap(f: A => THIS): THIS = {
    items.map(f).safeReduce(_ ++ _)(empty)
  }

  protected def empty: THIS

  protected def wrap(items: A*): THIS

  def getItems: List[A] = items

  def ++(other: THIS): THIS = {
    wrap(this.getItems ++ other.getItems: _*)
  }

  def ++(other: A): THIS = {
    wrap(this.getItems :+ other: _*)
  }

  def iterator: Iterator[A] = items.toIterator

  def apply(idx: Int): A = items.toArray.apply(idx)

  def length: Int = items.length
}
