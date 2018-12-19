package thebrains.youradventure.Adventure

class Location(
  name:           String,
  description:    String,
  parentLocation: Option[Location]
) extends Things(name, description)

object Location {

  case object Void
      extends Location(
        name = "void",
        description = "This is nowhere",
        None
      )

  case object Multiverse
      extends Location(
        name = "Multiverse",
        description = "This is the multiverse",
        None
      )

  case object OurUniverse
      extends Location(
        name = "Universe1",
        description = "This is our universe",
        parentLocation = Some(Multiverse)
      )

  case object MilkyWay
      extends Location(
        name = "MilkyWay",
        description = "The milky way galaxy",
        parentLocation = Some(OurUniverse)
      )

  case object SolarSystem
      extends Location(
        name = "Solar System",
        description = "This a system of planet",
        parentLocation = Some(MilkyWay)
      )

  case object Earth
      extends Location(
        name = "Earth",
        description = "The blue planet, planet Earth",
        parentLocation = Some(SolarSystem)
      )
}
