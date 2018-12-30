package thebrains.youradventure.Adventure.ConditionPack

import thebrains.youradventure.Adventure._
import thebrains.youradventure.ParentTest

class ConditionTest extends ParentTest {
  "Condition" - {
    val player = PlayerBuilder.Empty

    "Race condition" - {
      val condition = Condition(RaceCondition(Races.Human))
      "Test" in {
        for {
          isCondition <- condition.isTrueFor(player)
        } yield {
          assert(isCondition)
        }

      }
    }
  }
}
