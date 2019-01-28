package thebrains.youradventure.Utils

import thebrains.youradventure.ParentTest

class ErrTest extends ParentTest {
  "Error" - {
    "Should not be fatal by default" in {
      val e = Err("name", "description")
      assertFalse(e.isFatal)
    }

    "Copy" - {
      val e = Err("name", "description", isFatal = true)
      "Should update name" in {
        val newName = "new name"
        val newE = e.copy(name = newName)
        assertEquals(newName, newE.getName)
      }

      "Should update description" in {
        val newDescription = "new desc"
        val newE = e.copy(description = newDescription)
        assertEquals(newDescription, newE.getDescription)
      }
    }

    "Fatal" - {
      "Should be fatal" in {
        val e = Err("name", "description", isFatal = true)
        assert(e.isFatal)
      }

      "Should be fatal when fatal" in {
        val e = FatalError("name", "description")
        assert(e.isFatal)
      }

      "Should be created from Throwable" in {
        val errorMessage = "This is an error message"
        val ex = new Exception(errorMessage)
        val e = FatalError(ex)
        assert(e.isFatal)
        assertEquals(errorMessage, e.getDescription)
      }
    }

    "IO" - {
      "Should be converted to IO" in {
        val e = Err("name", "description")
        val io = unsafeRunToEither(e.toIO[String])
        assert(io.isLeft)
        val eIo = io.left.get
        assert(eIo.getMessage.contains(e.toString))
      }

      "Should be created as IO" in {
        ErrorIO("name", "description").shouldFail
      }
    }
  }
}
