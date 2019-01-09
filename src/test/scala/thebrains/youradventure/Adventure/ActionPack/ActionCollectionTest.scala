package thebrains.youradventure.Adventure.ActionPack

import scalaz.Maybe
import thebrains.youradventure.Adventure.StepPack.Steps
import thebrains.youradventure.FactoriesTest.ActionPack
import thebrains.youradventure.FactoriesTest.ActionPack.{FAction, FActionCollection}
import thebrains.youradventure.ParentTest
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
      val question = Maybe.just("The real original question !")
      val collection: ActionCollection = FActionCollection(
        question = question,
        lengthAction = 2.just
      )

      "Should have correct length" in {
        assertEquals(2, collection.length)
        assertEquals(question, collection.getQuestion)
      }

      "Should be able to add action" in {
        val collectionWithA = collection ++ a
        val expectedLength = 3

        assertEquals(question, collection.getQuestion)
        assertEquals(expectedLength, collectionWithA.length)
        assertEquals(a.getName.just, collectionWithA.get(2).map(_.getName))
      }

      "Should add other collection" in {
        val otherCollection = ActionCollection(a, "question")
        val collectionWithA = collection ++ otherCollection
        val expectedLength = 3

        assertEquals(question, collection.getQuestion)
        assertEquals(expectedLength, collectionWithA.length)
        assertEquals(a.getName.just, collectionWithA.get(2).map(_.getName))
      }

      "Should have the correct indexing" in {
        val biggerCollection = a ++ collection ++ b
        val expectedIndexOfB = 3

        assertEquals(question, collection.getQuestion)
        assertEquals(a.getName.some, biggerCollection.getIndexedActionsMap.get(0).map(_.getName))
        assertEquals(
          b.getName.some,
          biggerCollection.getIndexedActionsMap.get(expectedIndexOfB).map(_.getName)
        )
      }

      "empty" - {
        "Should have 0 length" in {
          assertEquals(0, ActionCollection.Empty.length)
        }

        "Should be empty from within the class" in {
          class CollectionTest
              extends ActionCollection(
                question = "questions".just,
                actions = List(
                  FAction(),
                  FAction()
                )
              ) {
            def emptyPublic: ActionCollection = this.empty
          }

          val collection: CollectionTest = new CollectionTest()
          assertEquals(0, collection.emptyPublic.length)
        }
      }

      "action" - {
        "Should find action by name" in {
          val actionA = unsafeRunToEither((collection ++ a).getAction(a.getName)).extract
          assertEquals(a.getName, actionA.getName)
        }

        "Should find action by index" in {
          val actionA = unsafeRunToEither((collection ++ a).getAction("2")).extract
          assertEquals(a.getName, actionA.getName)
        }

        "Should failed when name not found" in {
          collection.getAction("not a real action").shouldFail
        }

        "Should failed when index not found" in {
          collection.getAction("999").shouldFail
        }

        "Should failed when empty action name" in {
          collection.getAction("").shouldFail
        }
      }
    }

    "Bastard Action Collection" - {
      "empty" - {
        "Should have 0 length" in {
          assertEquals(0, BastardActionCollection.Empty.length)
        }

        "Should be empty from within the class" in {
          class CollectionTest
              extends BastardActionCollection(
                actions = List(
                  FAction(),
                  FAction()
                )
              ) {
            def emptyPublic: BastardActionCollection = this.empty
          }

          val collection: CollectionTest = new CollectionTest()
          assertEquals(0, collection.emptyPublic.length)
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
