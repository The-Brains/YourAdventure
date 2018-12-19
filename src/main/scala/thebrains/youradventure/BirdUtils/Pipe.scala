package thebrains.youradventure.BirdUtils

import scala.language.implicitConversions

class Pipe[A](a: A) extends Serializable {
  def |>[B](f: A => B): B = f(a)
}

object Pipe extends Serializable {
  def apply[A](v: A): Pipe[A] = new Pipe(v)
}

object PipeOps extends Serializable {
  implicit def toPipe[A](a: A): Pipe[A] = Pipe(a)
}
