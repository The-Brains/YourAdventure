package thebrains.youradventure.Adventure.ActionPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.StepPack.Steps
import thebrains.youradventure.{FactoriesTest, ParentTest}
import thebrains.youradventure.Utils.ToOption._

class ActionCollectionTest extends ParentTest {
  "ActionCollection" - {
    val a = FactoriesTest.FAction(
      name = "action A name".just,
      targetStep = Maybe.just(Right(Steps.EmptyStep))
    )
    val b = FactoriesTest.FAction(
      name = "action B name".just,
      targetStep = Maybe.just(Left("Exit"))
    )

    "Action Collection" - {
      "empty" - {
        "Should have 0 length" in {
          assertEquals(0, ActionCollection.Empty.length)
        }
      }

      val collection: ActionCollection = FactoriesTest.FActionCollection(
        lengthAction = 3.just
      )

    }

    "Bastard Action Collection" - {
      "empty" - {
        "Should have 0 length" in {
          assertEquals(0, BastardActionCollection.Empty.length)
        }
      }

      val collection: BastardActionCollection =
        BastardActionCollection(FactoriesTest.FAction(), FactoriesTest.FAction())

      "Should have correct length" in {
        assertEquals(2, collection.length)
      }

      "Should be able to add action" in {
        val collectionWithA = collection ++ a
        val expectedLength = 3
        assertEquals(expectedLength, collectionWithA.length)
        assertEquals(a.getName.just, collectionWithA.get(2).map(_.getName))
      }

      "Should have the correct indexing" in {
        val biggerCollection = a ++ collection ++ b
        val expectedIndexOfB = 3
        assertEquals(a.getName.some, biggerCollection.getIndexedActionsMap.get(0).map(_.getName))
        assertEquals(
          b.getName.some,
          biggerCollection.getIndexedActionsMap.get(expectedIndexOfB).map(_.getName)
        )
      }
    }
  }
}
