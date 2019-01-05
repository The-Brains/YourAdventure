package thebrains.youradventure.Adventure.CollectionPack

import scalaz.zio.IO
import thebrains.youradventure.Utils.{Error, FatalError}
import thebrains.youradventure.Adventure.Things

abstract class AssemblyItemTrait(
  name:        String,
  description: String
) extends Things(name, description) {
  def |+|(other: AssemblyItemTrait): IO[Error, AssemblyItemTrait] =
    IO.fail(FatalError("Cannot combine", "You cannot combine those 'AssemblyItemTrait'."))
}
