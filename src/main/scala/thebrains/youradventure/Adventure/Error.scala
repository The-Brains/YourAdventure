package thebrains.youradventure.Adventure

case class Error(
  name:        String,
  description: String
) extends Things(name, description)

object Error {
  val Empty: Error = Error("", "")
}
