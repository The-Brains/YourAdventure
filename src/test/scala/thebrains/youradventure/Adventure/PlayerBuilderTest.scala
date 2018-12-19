package thebrains.youradventure.Adventure

import thebrains.youradventure.Adventure.AttributePack.Attributes
import thebrains.youradventure.ParentTest

class PlayerBuilderTest extends ParentTest {
  "Player" - {
    "Should be created" - {
      "with builder" in {
        val player = PlayerBuilder
          .create("john")
          .selectRace(Races.Human)

        assertEquals("john", player.name)
        assertEquals(
          Races.Human.baseAttributes.getAttributeValue(Attributes.Strength),
          player.currentAttributes.getAttributeValue(Attributes.Strength)
        )
      }
    }
  }
}
