package thebrains.youradventure.Adventure.CollectionPack

import scalaz.Maybe
import thebrains.youradventure.Utils.Error

import scala.reflect.ClassTag

abstract class AssemblyTrait[A <: AssemblyItemTrait : ClassTag](items: A*) {
  protected def toCustomMap: Map[String, A] = {
    items.map(a => (a.getName, a)).toMap
  }

  protected def combine(a: A, b: A): Either[Error, A]

  protected def reduceAll: Either[Error, A] = {
    items
      .foldLeft[Either[Error, A]](Left(Error.Empty)) {
      case (Right(a), b) => combine(a, b)
      case (Left(_), b) => Right(b)
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
