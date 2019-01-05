package thebrains.youradventure.Utils

import java.io.IOException

import scalaz.Maybe
import thebrains.youradventure.Adventure.Things
import ToOption._

class Error(
  name:        String,
  description: String,
  fatal:       Boolean,
  stack:       List[StackTraceElement]
) extends Things(name, description) {
  @transient lazy val toDisplay: String = s"Error: $name - $description"

  @transient lazy val isFatal: Boolean = this.fatal

  @transient lazy val stackAsString: Maybe[String] = (fatal, stack) match {
    case (false, _) => Maybe.empty
    case (_, Nil)   => Maybe.empty
    case (true, e)  => ("\n" + e.map(s => s.toString).mkString("\n")).just
  }

  def copy(
    name:        String = this.name,
    description: String = this.description,
    fatal:       Boolean = this.fatal,
    stack:       List[StackTraceElement] = this.stack
  ): Error = {
    new Error(name, description, fatal, stack)
  }
}

case class FatalError(
  name:        String,
  description: String,
  stackTrace:  List[StackTraceElement] = Thread.getAllStackTraces.get(Thread.currentThread()).toList
) extends Error(
      name,
      description,
      fatal = true,
      stackTrace
    )

object Error {
  val Empty: Error = new Error(name = "", description = "", fatal = false, Nil)

  def apply(
    name:        String,
    description: String,
    isFatal:     Boolean = false
  ): Error = {
    new Error(
      name,
      description,
      isFatal,
      Thread.getAllStackTraces.get(Thread.currentThread()).toList
    )
  }

  private def createFrom(ex: Throwable): FatalError = {
    FatalError(ex.getClass.getCanonicalName, ex.getMessage, ex.getStackTrace.toList)
  }

  def apply(ex: IOException): Error = createFrom(ex)

}
