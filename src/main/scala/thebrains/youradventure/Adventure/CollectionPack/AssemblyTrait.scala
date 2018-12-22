package thebrains.youradventure.Adventure.CollectionPack

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Utils.Error

import scala.reflect.ClassTag

abstract class AssemblyTrait[A <: AssemblyItemTrait : ClassTag](items: A*) {
  def toCustomMap: Map[String, A] = {
    items.map(a => (a.getName, a)).toMap
  }

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
      case head :: second :: Nil => (head |+| second).flatMap {
        case a: A => IO.sync(a)
        case _ => IO.fail(Error("Cannot convert", "Cannot convert to 'A'."))
      }
      case head :: Nil => IO.sync(head)
      case Nil => IO.fail(Error("Empty list", "There is nothing to combine"))
    }
  }

  def find(p: A => Boolean): Maybe[A] = Maybe.fromOption(items.find(p))

  def map(f: A => A): AssemblyTrait[A] = wrap(items.map(f): _*)

  def flatMap(f: A => AssemblyTrait[A]): AssemblyTrait[A] = {
    items.map(f).reduce(_ ++ _)
  }

  protected def wrap(items: A*): AssemblyTrait[A]

  def getItems: Seq[A] = items

  def ++(other: AssemblyTrait[A]): AssemblyTrait[A] = {
    wrap(this.getItems ++ other.getItems: _*)
  }

  def ++(other: A): AssemblyTrait[A] = {
    wrap(this.getItems :+ other: _*)
  }

  def iterator: Iterator[A] = items.toIterator

  def apply(idx: Int): A = items.toArray.apply(idx)

  def length: Int = items.length
}
