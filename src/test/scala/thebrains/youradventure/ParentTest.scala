package thebrains.youradventure

import org.scalactic.source.Position
import org.scalatest._
import scalaz.zio.RTS

class ParentTest extends FreeSpec with RTS {
  def assertEquals[A](
    expected: A,
    result:   A
  )(
    implicit pos: Position
  ): Assertion = {
    assertResult(expected)(result)
  }

  override protected def runTest(
    testName: String,
    args:     Args
  ): Status = {
    println(s">>> Starting '$testName'")

    val output = super.runTest(testName, args)

    println(s"<<< Done '$testName'")
    output
  }
}
