package thebrains.youradventure.Adventure.CollectionPack

import thebrains.youradventure.ParentTest

class AssemblyItemTraitTest extends ParentTest {
  "AssemblyItemTrait" - {
    "Merge" - {
      "Should fail without override" in {
        case class Test() extends AssemblyItemTrait("name", "description") {}
        val t1 = Test()
        val t2 = Test()

        val output = unsafeRunToEither(t1 |+| t2)
        assert(output.isLeft)
      }
    }
  }
}
