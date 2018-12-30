package thebrains.youradventure.Adventure.ActionPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.StepPack.Steps
import thebrains.youradventure.{FactoriesTest, ParentTest}

class ActionCollectionTest extends ParentTest {
  "ActionCollection" - {
    val a = FactoriesTest.FAction(
      name = Maybe.just("action A name"),
      targetStep = Maybe.just(Right(Steps.EmptyStep))
    )
    val b = FactoriesTest.FAction(
      name = Maybe.just("action B name"),
      targetStep = Maybe.just(Left("Exit"))
    )

    "Action Collection" - {}
    "Bastard Action Collection" - {
      val collection: BastardActionCollection =
        BastardActionCollection(FactoriesTest.FAction(), FactoriesTest.FAction())

      "Should have correct length" in {
        assertEquals(2, collection.length)
      }

      "Should be able to add action" in {
        val collectionWithA = collection ++ a
        val expectedLength = 3
        assertEquals(expectedLength, collectionWithA.length)
        assertEquals(Maybe.just(a.getName), collectionWithA.get(2).map(_.getName))
      }
      "Should have the correct indexing" in {
        val biggerCollection = a ++ collection ++ b
        val expectedIndexOfB = 3
        assertEquals(Some(a.getName), biggerCollection.getIndexedActionsMap.get(0).map(_.getName))
        assertEquals(
          Some(b.getName),
          biggerCollection.getIndexedActionsMap.get(expectedIndexOfB).map(_.getName)
        )
      }
    }
  }
}
