package thebrains.youradventure.Adventure.CollectionPack

import thebrains.youradventure.ParentTest

class ToSortedSetTest extends ParentTest {
  "ToSortedSet" - {
    import ToSortedSet._
    "From List" - {
      "Should sort" in {
        val l = List(10, 1, 2)
        assertEquals(List(1, 2, 10), l.toSortedSet.toList)
      }

      "Should distinct" in {
        val l = List(1, 1, 1, 1)
        assertEquals(List(1), l.toSortedSet.toList)
      }

      "Should sort and distinct" in {
        val l = List(1, 10, 2, 2, 1, 1, 2, 10)
        assertEquals(List(1, 2, 10), l.toSortedSet.toList)
      }
    }
  }
}
