package thebrains.youradventure.Adventure

import io.circe.{Encoder, Json}
import scalaz.zio.IO
import thebrains.youradventure.Adventure.AttributePack.AttributeCollection
import thebrains.youradventure.Adventure.BodyPack.PlayerBodyPart
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.Utils.Error
import io.circe.generic.auto._
import io.circe.syntax._
import scalaz.Maybe

case class Player(
  name:           String,
  journey:        List[Step],
  consumables:    List[Consumable],
  bodyParts:      List[PlayerBodyPart],
  baseAttributes: AttributeCollection,
  race:           Race
) extends PlayerTrait {
  private def equipments: List[Maybe[Equipment]] = bodyParts.map(_.equipment).filter(_.isJust)

  private def equipmentModifier: TransformationCollection = {
    equipments
      .map(_.map(_.modifiers))
      .foldLeft[TransformationCollection](TransformationCollection.Empty) {
        case (tc, Maybe.Just(tc2)) => tc ++ tc2
        case (tc, Maybe.Empty())   => tc
      }
  }

  def currentAttributes: IO[Error, AttributeCollection] = baseAttributes << equipmentModifier

  def toStatus: String = ""

  def addHistory(s: Step): IO[Nothing, Player] = {
    IO.sync(this.copy(journey = journey :+ s))
  }

  implicit private val jsonEncoder: Encoder[Player] =
    Encoder
      .forProduct6[Player, String, List[Json], List[Json], List[Json], List[Json], Json](
        "name",
        "journey",
        "consumables",
        "bodyParts",
        "attributes",
        "race"
      ) { p: Player =>
        (
          p.name,
          p.journey.map(_.encoded),
          p.consumables.map(_.encoded),
          p.bodyParts.map(_.encoded),
          p.baseAttributes.encoded,
          p.race.encoded
        )
      }

  override def encoded: Json = this.asJson

  override def toString: String = this.asJson.noSpaces
}

sealed trait PlayerTrait {
  def encoded: Json
}

object PlayerBuilder {
  val NameQuestion: String = "What is your name?"
  val RaceQuestion: String = "What race are you part of ?"

  case class PlayerWithName(name: String) extends PlayerTrait {
    def selectRace(race: Race): Player = {
      Player(
        name = name.trim,
        journey = Nil,
        consumables = Nil,
        bodyParts = race.bodyParts.map(_.toPlayerBodyPart),
        baseAttributes = race.baseAttributes,
        race = race
      )
    }

    def selectRace(availableRaces: List[Race])(race: String): IO[Error, Player] = {
      availableRaces.find(_.getLowerCaseName == race.toLowerCase) match {
        case Some(r) => IO.sync(selectRace(r))
        case None =>
          IO.fail(
            Error(
              "Not found race",
              s"Could not found '$race' among: " +
                s"${availableRaces.map(_.getCapitalizeName).mkString(", ")}"
            )
          )
      }
    }

    override def encoded: Json = name.asJson

    override def toString: String = s"'$name'"
  }

  def create(name: String): IO[Nothing, PlayerWithName] = {
    IO.sync(PlayerWithName(name))
  }
}
