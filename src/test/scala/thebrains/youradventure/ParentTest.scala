package thebrains.youradventure

import org.scalactic.source.Position
import org.scalatest.{Assertion, FreeSpec}

class ParentTest extends FreeSpec {
  def assertEquals[A](
    expected: A,
    result:   A
  )(
    implicit pos: Position
  ): Assertion = {
    assertResult(expected)(result)
  }
}
