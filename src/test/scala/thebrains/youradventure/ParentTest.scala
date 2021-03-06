package thebrains.youradventure

import org.scalactic.source.Position
import org.scalatest._
import scalaz.zio.{IO, RTS}

import scala.util.Random

class ParentTest extends FreeSpec with RTS {
  implicit val r: Random = new Random(0)

  def unsafeRunToEither[E, I](io: IO[E, I]): Either[Throwable, I] = unsafeRunSync(io).toEither

  def assertFalse(condition: Boolean)(implicit pos: Position): Assertion = {
    assert(!condition)
  }

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
//    println(s">>> Starting '$testName'")

    val output = super.runTest(testName, args)

//    println(s"<<< Done '$testName'")
    output
  }
}
