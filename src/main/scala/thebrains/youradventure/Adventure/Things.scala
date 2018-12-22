package thebrains.youradventure.Adventure

class Things(
  name:        String,
  description: String
) {
  override def toString: String = s"'$name'"

  def ===(other: Things): Boolean = this.getName == other.getName

  override def hashCode(): Int = getName.hashCode

  override def equals(obj: Any): Boolean = {
    obj match {
      case p: Things => p === this
      case _ => false
    }
  }

  lazy val getDescription: String = description

  lazy val getName: String = name

  lazy val getLowerCaseName: String = name.toLowerCase

  lazy val getCapitalizeName: String = name.capitalize
}
