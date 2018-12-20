package thebrains.youradventure.Utils.BirdUtils

// https://hackernoon.com/operator-in-scala-cbca7b939fc0
object BirdOperator extends Serializable {

  implicit class Pipe[A](val a: A) extends AnyVal with Serializable {
    def |>[Z](f: A => Z): Z = f(a)
  }

  implicit class Pipe2[A, B](val a: (A, B)) extends AnyVal with Serializable {
    def |>[Z](f: (A, B) => Z): Z = f(a._1, a._2)
  }

}
