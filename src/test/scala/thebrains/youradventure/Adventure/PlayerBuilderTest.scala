package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.AttributePack.Attributes
import thebrains.youradventure.ParentTest

class PlayerBuilderTest extends ParentTest {
  "Player" - {
    "Should be created" - {
      "with builder" in {
        val player = unsafeRun(PlayerBuilder.create("john"))
          .selectRace(Races.Human)

        assertEquals("john", player.getName)
        assertEquals(
          Races.Human.baseAttributes.getAttributeValue(Attributes.Strength),
          unsafeRun(player.currentAttributes).getAttributeValue(Attributes.Strength)
        )
      }
    }
  }
}
