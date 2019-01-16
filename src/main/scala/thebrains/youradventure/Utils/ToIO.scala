package thebrains.youradventure.Utils

import scalaz.zio.IO

object ToIO {
  implicit class ConvertToIO[A](a: A) {
    def toIO: IO[Err, A] = IO.sync(a)
  }
}
