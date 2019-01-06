package thebrains.youradventure.Adventure.CollectionPack

import scalaz.Maybe
import thebrains.youradventure.ParentTest
import thebrains.youradventure.Utils.ToOption._
import thebrains.youradventure.Utils.Err
import ListImplicits._
import org.scalatest.Assertion
import scalaz.zio.IO

class ListImplicitsTest extends ParentTest {
  "ListImplicit" - {
    "isUnique" - {
      "Should be unique" in {
        assert(List(1, 2).isUnique)
      }

      "Should not be unique" in {
        assertFalse(List(1, 1, 2, 2).isUnique)
      }
    }

    "getExtras" - {
      "Should return right list" in {
        assertEquals(List(2, 2, 10), List(1, 2, 2, 2, 10, 10).getExtras)
      }
    }

    "getExtrasDistinct" - {
      "Should return right list" in {
        assertEquals(List(2, 10), List(1, 2, 2, 2, 10, 10).getExtrasDistinct)
      }
    }

    "safeReduce" - {
      "Should return default" in {
        assertEquals(10, List[Int]().safeReduce(_ + _)(10))
      }

      "Should return reduce" in {
        assertEquals(2, List[Int](1, 1).safeReduce(_ + _)(10))
      }
    }

    "headMaybe" - {
      "Should return Empty" in {
        assertEquals(Maybe.empty, List().headMaybe)
      }

      "Should return head" in {
        assertEquals(1.just, List(1.just, 2.just).headMaybe)
      }
    }

    "filterMap" in {
      assertEquals(List(1, 10, 1, 10), List(1, 2, 1, 2).filterMap(_ % 2 == 0)(_ * 5))
    }

    "updateFirst" in {
      assertEquals(List(1, 10, 1, 2), List(1, 2, 1, 2).updateFirst(_ % 2 == 0)(_ * 5))
      assertEquals(List(1, 2, 1, 2), List(1, 2, 1, 2).updateFirst(_ > 10)(_ * 5))
    }

    "updateFirstIO" in {
      def test[A](
        l:              List[A],
        p:              A => Boolean,
        operation:      A => IO[Err, A],
        expectedResult: List[A]
      ): Assertion = {
        val io = IO.sequence(l.updateFirstIO(p)(operation))
        val output = unsafeRunToEither(io)
        assert(output.isRight)
        val result = output.right.get
        assertEquals(expectedResult, result)
      }

      test[Int](
        l = List(1, 2, 1, 2),
        p = _ % 2 == 0,
        operation = i => IO.sync(i * 5),
        expectedResult = List(1, 10, 1, 2)
      )

      test[Int](
        l = List(1, 2, 1, 2),
        p = _ > 10,
        operation = i => IO.sync(i * 5),
        expectedResult = List(1, 2, 1, 2)
      )
    }
  }

  "OptionMaybe" - {
    "headMaybe" - {
      "Should be empty if no head" in {
        assert(List[Int]().headMaybe.isEmpty)
      }

      "Should be not empty is head" in {
        assert(List(1).headMaybe.isJust)
      }
    }
  }
}
