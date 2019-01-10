package thebrains.youradventure

import org.scalactic.source.Position
import org.scalatest._
import scalaz.zio.{IO, RTS}
import thebrains.youradventure.Utils.Err

import scala.util.Random

class ParentTest extends FreeSpec with RTS {

  implicit class Extractor[I](e: Either[Throwable, I]) {
    def extract(implicit pos: Position): I = {
      def errorMessage: String = {
        if (e.isLeft) {
          s"IO Failed: ${e.left.get}"
        } else {
          s"Something went wrong."
        }
      }

      assert(e.isRight, errorMessage)
      e.right.get
    }
  }

  implicit class ExtractorRaw[I](e: IO[Err, I]) {
    def extract(implicit pos: Position): I = unsafeRunToEither(e).extract

    def shouldFail(implicit pos: Position): Assertion = {
      val io = unsafeRunToEither(e)
      assert(io.isLeft)
    }
  }

  implicit class ContainsALot(txt: String) {
    def containsAll(strings: List[String])(implicit pos: Position): Seq[Assertion] = {
      strings.map { s =>
        assert(txt.contains(s), s"Input '$txt' does not contains '$s'")
      }
    }
  }

  implicit val randomImplicitValue: Random = new Random(0)

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
