package thebrains.youradventure.Adventure

import thebrains.youradventure.FactoriesTest.FRace
import thebrains.youradventure.FactoriesTest.StepPack.FStep
import thebrains.youradventure.ParentTest
import thebrains.youradventure.Utils.ToOption._

class PlayerBuilderTest extends ParentTest {
  "Player" - {
    "Should be created" - {
      "with builder" in {
        val race = FRace()
        val attribute = race.getBaseAttributes.getItems.head
        val player = PlayerBuilder.create("john").extract.selectRace(race)

        assertEquals("john", player.getName)
        assertEquals(
          attribute.getValue.just,
          player.currentAttributes.extract.getAttributeValue(attribute.getAttribute)
        )
      }
    }

    "History" - {
      "Should be able to add step" in {
        val player = PlayerBuilder.create("john").extract.selectRace(Races.Human)
        val step = FStep()
        val newPlayer = player.addHistory(step).extract
        val foundStep = newPlayer.getJourney.getStep(step.getName).extract
        assertEquals(step.getName, foundStep.getName)
      }
    }

    "Encoding" - {
      "Should have the right information" in {
        val race = FRace()
        val name = "name of the character"
        val player = PlayerBuilder.create(name).extract.selectRace(race)
        val json = player.toString

        json.containsAll(
          List(name, race.getName) ++
            race.getBaseAttributes.outMap(_.getName) ++
            race.getBodyParts.outMap(_.getName)
        )
      }
    }

    "PlayerWithName" - {
      "Race" - {
        val playerWithName: PlayerBuilder.PlayerWithName = PlayerBuilder.create("leo").extract

        "Should be finding race" in {
          val r = FRace()
          val availableRace = List(FRace(), r, FRace(), FRace())

          val player = playerWithName.selectRace(availableRace)(r.getName).extract
          assertEquals(r.getName, player.getRace.getName)
        }

        "Should be failing to find race" in {
          val r = FRace()
          val availableRace = List(FRace(), FRace(), FRace())

          playerWithName.selectRace(availableRace)(r.getName).shouldFail
        }
      }
    }
  }
}
