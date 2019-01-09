package thebrains.youradventure.FPTerminalIO

import thebrains.youradventure.ParentTest

class InputTest extends ParentTest {
  "Input" - {
    "Should return content" in {
      val i = Input("message")
      val io = unsafeRunToEither(i.getContent)
      val content = io.extract
      assertEquals("message", content.input)
    }

    "Should return emptyContent" in {
      val i = Input()
      val io = unsafeRunToEither(i.getContent)
      assert(io.isLeft)
    }
  }
}
