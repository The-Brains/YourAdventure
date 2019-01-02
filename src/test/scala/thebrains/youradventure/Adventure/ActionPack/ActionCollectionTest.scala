package thebrains.youradventure.Adventure.ActionPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.StepPack.Steps
import thebrains.youradventure.FactoriesTest.ActionPack
import thebrains.youradventure.FactoriesTest.ActionPack.{FAction, FActionCollection}
import thebrains.youradventure.{FactoriesTest, ParentTest}
import thebrains.youradventure.Utils.ToOption._

class ActionCollectionTest extends ParentTest {
  "ActionCollection" - {
    val a = FAction(
      name = "action A name".just,
      targetStep = Maybe.just(Right(Steps.EmptyStep))
    )
    val b = ActionPack.FAction(
      name = "action B name".just,
      targetStep = Maybe.just(Left("Exit"))
    )

    "Action Collection" - {
      "empty" - {
        "Should have 0 length" in {
          assertEquals(0, ActionCollection.Empty.length)
        }
      }

      val collection: ActionCollection = FActionCollection(
        lengthAction = 2.just
      )

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
      "action" - {
        "Should find action by name" in {
          val actionA = unsafeRunToEither((collection ++ a).getAction(a.getName))
          assert(actionA.isRight)
          assertEquals(a.getName, actionA.right.get.getName)
        }

        "Should find action by index" in {
          val actionA = unsafeRunToEither((collection ++ a).getAction("2"))
          assert(actionA.isRight)
          assertEquals(a.getName, actionA.right.get.getName)
        }

        "Should failed when name not found" in {
          val actionA = unsafeRunToEither(collection.getAction("not a real action"))
          assert(actionA.isLeft)
        }

        "Should failed when index not found" in {
          val actionA = unsafeRunToEither(collection.getAction("999"))
          assert(actionA.isLeft)
        }

        "Should failed when empty action name" in {
          val actionA = unsafeRunToEither(collection.getAction(""))
          assert(actionA.isLeft)
        }
      }
    }

    "Bastard Action Collection" - {
      "empty" - {
        "Should have 0 length" in {
          assertEquals(0, BastardActionCollection.Empty.length)
        }
      }

      val collection: BastardActionCollection =
        BastardActionCollection(ActionPack.FAction(), ActionPack.FAction())

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
