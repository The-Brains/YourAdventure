package thebrains.youradventure.Adventure

import scalaz.Maybe
import thebrains.youradventure.Utils.ToOption._

class Location(
  name:           String,
  description:    String,
  parentLocation: Maybe[Location]
) extends Things(name, description)

object Locations {

  final case object Menu
      extends Location(
        name = "Menu Interface",
        description = "This is a Menu",
        Maybe.empty
      )

  final case object Void
      extends Location(
        name = "void",
        description = "This is nowhere",
        Maybe.empty
      )

  final case object Multiverse
      extends Location(
        name = "Multiverse",
        description = "This is the multiverse",
        Maybe.empty
      )

  final case object OurUniverse
      extends Location(
        name = "Universe1",
        description = "This is our universe",
        parentLocation = Maybe.just(Multiverse)
      )

  final case object MilkyWay
      extends Location(
        name = "MilkyWay",
        description = "The milky way galaxy",
        parentLocation = Maybe.just(OurUniverse)
      )

  final case object SolarSystem
      extends Location(
        name = "Solar System",
        description = "This a system of planet",
        parentLocation = Maybe.just(MilkyWay)
      )

  final case object Earth
      extends Location(
        name = "Earth",
        description = "The blue planet, planet Earth",
        parentLocation = Maybe.just(SolarSystem)
      )

}
