package thebrains.youradventure.Adventure

import thebrains.youradventure.ParentTest

class RaceTest extends ParentTest {
  "Race" - {
    "Find" - {
      "Should return error when not found" in {
        val result = unsafeRunToEither(Races.fromString("not a valid race"))
        assert(result.isLeft)
      }

      "Should find when valid" in {
        val result = unsafeRunToEither(Races.fromString(Races.Human.getName))
        val race = result.extract
        assertEquals(Races.Human.getName, race.getName)
      }
    }
  }
}
