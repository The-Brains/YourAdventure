package thebrains.youradventure.Utils

object ToEither {

  implicit class ConvertToEither[A](a: A) {
    def asLeft[O]: Either[A, O] = Left(a)

    def asRight[O]: Either[O, A] = Right(a)
  }

}
