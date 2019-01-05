package thebrains.youradventure.Adventure

import io.circe.Json
import io.circe.syntax._

class Things(
  name:        String,
  description: String
) {
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

  lazy val getDescription: String = description.trim

  lazy val getName: String = name.trim

  lazy val getLowerCaseName: String = name.toLowerCase

  lazy val getCapitalizeName: String = name.capitalize
}
