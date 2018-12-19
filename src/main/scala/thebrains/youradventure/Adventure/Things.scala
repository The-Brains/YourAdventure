package thebrains.youradventure.Adventure

class Things(
  name:        String,
  description: String
) {
  override def toString: String = s"'$name'"

  lazy val getDescription: String = description

  lazy val getName: String = name

  lazy val getLowerCaseName: String = name.toLowerCase

  lazy val getCapitalizeName: String = name.capitalize
}
