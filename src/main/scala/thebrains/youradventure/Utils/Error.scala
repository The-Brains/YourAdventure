package thebrains.youradventure.Utils

import java.io.IOException

import thebrains.youradventure.Adventure.Things

class Error(
  name:        String,
  description: String,
  fatal:       Boolean
) extends Things(name, description) {
  @transient lazy val toDisplay: String = s"Error: $name - $description"

  @transient lazy val isFatal: Boolean = this.fatal
}

object Error {
  val Empty: Error = new Error(name = "", description = "", fatal = false)

  def apply(
    name:        String,
    description: String,
    isFatal:     Boolean = false
  ): Error = {
    new Error(name, description, isFatal)
  }

  case class FatalError(
    name:        String,
    description: String
  ) extends Error(
        name,
        description,
        fatal = true
      )

  private def createFrom(ex: Throwable): Error = {
    FatalError(
      ex.getClass.getCanonicalName,
      ex.getMessage
    )
  }

  def apply(ex: IOException): Error = {
    this.createFrom(ex)
  }

}
