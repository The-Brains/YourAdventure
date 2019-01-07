package thebrains.youradventure.Adventure

import thebrains.youradventure.ParentTest

class ThingsTest extends ParentTest {
  "Things" - {
    "Equals" - {
      val name = "name"
      val a = new Things(name, "description")

      "Should be equals" in {
        val b = a
        assertEquals(a, b)
        assert(a === b)
        assert(a == b)
      }

      "Should be equals with different description" in {
        val b = new Things(name, "something else")
        assertEquals(b, a)
        assert(a === b)
        assert(a == b)
      }

      "Should not be equals with other object" in {
        val c = "String"
        val d = 2
        assertFalse(a.equals(c))
        assertFalse(a.equals(d))
      }
    }

    "Compare" - {
      val a = new Things("a", "description")
      val b = new Things("b", "other desc")

      "Should follow the name alphabetical order" in {
        assert(a < b)
        assert(b > a)
        assert(a.compare(b) < 0)
        assert(b.compare(a) > 0)
      }

      "Should be sorted" in {
        import thebrains.youradventure.Adventure.CollectionPack.ToSortedSet._

        val l = List(b, a)
        assertEquals(List(a, b), l.toSortedSet.toList)
      }
    }
  }
}
