package thebrains.youradventure.Adventure.CollectionPack

import thebrains.youradventure.Utils.Error
import thebrains.youradventure.Adventure.Things

abstract class AssemblyItemTrait(name: String, description: String)
  extends Things(name, description) {
  def |+|(other: AssemblyItemTrait): Either[Error, AssemblyItemTrait]
}
