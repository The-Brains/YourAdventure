package thebrains.youradventure.Utils

import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Adventure.Things
import thebrains.youradventure.Utils.ToOption._

class Err(
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
  ): Err = {
    new Err(name, description, fatal, stack)
  }

  def toIO[A]: IO[Err, A] = IO.fail(this)

  override def toString: String = s"[${super.toString} ($description)]"
}

case class FatalError(
  name:        String,
  description: String,
  stackTrace:  List[StackTraceElement] = FatalError.getStackTrace
) extends Err(
      name,
      description,
      fatal = true,
      stackTrace
    )

object Err {
  val Empty: Err = new Err(name = "", description = "", fatal = false, Nil)

  def apply(
    name:        String,
    description: String,
    isFatal:     Boolean = false
  ): Err = {
    new Err(
      name,
      description,
      isFatal,
      FatalError.getStackTrace
    )
  }
}

object FatalErrorIO {
  def apply[A](
    name:        String,
    description: String,
    stackTrace:  List[StackTraceElement] = FatalError.getStackTrace
  ): IO[FatalError, A] = {
    IO.fail(FatalError(name, description, stackTrace))
  }
}

object FatalError {

  def getStackTrace: List[StackTraceElement] = {
    Thread.getAllStackTraces.get(Thread.currentThread()).toList
  }

  private def createFrom(ex: Throwable): FatalError = {
    FatalError(ex.getClass.getCanonicalName, ex.getMessage, ex.getStackTrace.toList)
  }

  def apply(ex: Throwable): Err = createFrom(ex)
}

object ErrorIO {
  def apply[A](
    name:        String,
    description: String,
    isFatal:     Boolean = false
  ): IO[Err, A] = {
    IO.fail(Err(name, description, isFatal))
  }
}
