package thebrains.youradventure.Adventure

class Things(
  name:        String,
  description: String
) {
  override def toString: String = s"'$name'"

  def getDescription: String = description
}
