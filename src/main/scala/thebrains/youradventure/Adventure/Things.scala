package thebrains.youradventure.Adventure

import io.circe.Json
import io.circe.syntax._

class Things(
  name:        String,
  description: String
) extends Ordered[Things] {
  @transient lazy val getDescription:    String = description.trim
  @transient lazy val getName:           String = name.trim
  @transient lazy val getLowerCaseName:  String = name.toLowerCase
  @transient lazy val getCapitalizeName: String = name.capitalize

  // https://stackoverflow.com/a/19348339/3357831
  override def compare(that: Things): Int = {
    this.getName compare that.getName
  }

  def encoded: Json = name.asJson

  override def toString: String = s"'$name'"

  def ===(other: Things): Boolean = {
    this.getClass == other.getClass && this.getName == other.getName
  }

  override def hashCode(): Int = getName.hashCode

  override def equals(obj: Any): Boolean = {
    obj match {
      case p: Things => p === this
      case _ => false
    }
  }
}

object Things {
  // Note that because `Ordering[A]` is not contravariant, the declaration
  // must be type-parametrized in the event that you want the implicit
  // ordering to apply to subclasses of `Employee`.
  implicit def orderingByName[A <: Things]: Ordering[A] = {
    Ordering.by(_.getName)
  }

  val OrderingByDescription: Ordering[Things] = Ordering.by(_.getDescription)
}
