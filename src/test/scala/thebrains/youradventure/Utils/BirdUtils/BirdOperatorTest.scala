package thebrains.youradventure.Utils.BirdUtils

import thebrains.youradventure.ParentTest

class BirdOperatorTest extends ParentTest {
  "Bird pipe" - {
    import thebrains.youradventure.Utils.BirdOperator._
    "pipe1" - {

      def add2(a: Int): Int = a + 10

      "Should return piped result from method" in {
        def get1: Int = 1
        val expected = 11
        assertEquals(expected, get1 |> add2)
      }

      "Should return piped result from val" in {
        val input: Int = 10
        val expected = 20
        assertEquals(expected, input |> add2)
      }
    }

    "pipe2" - {
      def sum(
        a: Int,
        b: Int
      ): Int = a + b

      "Should return piped result from method" in {
        def get: (Int, Int) = (1, 2)
        val expected = 3
        assertEquals(expected, get |> sum)
      }

      "Should return piped result from val" in {
        val input: (Int, Int) = (10, 2)
        val expected = 12
        assertEquals(expected, input |> sum)
      }
    }
  }
}
