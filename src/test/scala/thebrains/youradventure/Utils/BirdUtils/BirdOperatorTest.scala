package thebrains.youradventure.Utils.BirdUtils

import thebrains.youradventure.ParentTest

class BirdOperatorTest extends ParentTest {
  "Bird pipe" - {
    import BirdOperator._
    "pipe1" - {
      def get1: Int = 1

      def add2(a: Int): Int = a + 2

      val expected = 3
      assertEquals(expected, get1 |> add2)
    }

    "pipe2" - {
      def get: (Int, Int) = (1, 2)
      def sum(
        a: Int,
        b: Int
      ): Int = a + b
      val expected = 3
      assertEquals(expected, get |> sum)
    }
  }
}
