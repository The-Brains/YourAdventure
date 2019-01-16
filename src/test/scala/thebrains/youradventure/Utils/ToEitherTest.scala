package thebrains.youradventure.Utils

import thebrains.youradventure.ParentTest

class ToEitherTest extends ParentTest {
  "ToEither" - {
    import ToEither._
    "Left" - {
      "Should be the left of it" in {
        assert("plop".asLeft.isLeft)
      }
    }

    "Right" - {
      "Should be the right of it" in {
        assert("plop".asRight.isRight)
      }
    }
  }
}
