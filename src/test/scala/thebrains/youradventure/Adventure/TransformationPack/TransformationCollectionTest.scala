package thebrains.youradventure.Adventure.TransformationPack

import thebrains.youradventure.FactoriesTest.TransformationPack.FTransformation
import thebrains.youradventure.ParentTest

class TransformationCollectionTest extends ParentTest {
  "TransformationCollection" - {
    "Empty" - {
      "Should be empty from within the class" in {
        class CollectionTest
            extends TransformationCollection(
              transformations = List(FTransformation(), FTransformation())
            ) {
          def emptyPublic: TransformationCollection = this.empty
        }

        val collection: CollectionTest = new CollectionTest()
        assertEquals(0, collection.emptyPublic.length)
      }
    }
  }
}
