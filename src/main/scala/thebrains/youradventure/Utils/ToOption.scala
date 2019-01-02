package thebrains.youradventure.Utils

import scalaz.Maybe

object ToOption {

  implicit class Converter[A](a: A) {
    def some: Some[A] = Some(a)

    def just: Maybe[A] = Maybe.just(a)
  }

}
