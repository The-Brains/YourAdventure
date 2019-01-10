package thebrains.youradventure.Adventure

import io.circe.syntax._
import io.circe.{Encoder, Json}
import scalaz.zio.IO
import thebrains.youradventure.Adventure.AttributePack.AttributeCollection
import thebrains.youradventure.Adventure.BodyPack.PlayerBodyCollection
import thebrains.youradventure.Adventure.StepPack.{Step, StepCollection}
import thebrains.youradventure.Adventure.TransformationPack.TransformationCollection
import thebrains.youradventure.Utils.{Err, ErrorIO}
import thebrains.youradventure.Adventure.CollectionPack.ListImplicits._

class Player(
  name:           String,
  journey:        StepCollection,
  consumables:    List[Consumable],
  bodyParts:      PlayerBodyCollection,
  baseAttributes: AttributeCollection,
  race:           Race
) extends PlayerTrait {
  @transient lazy val getName:            String = name
  @transient lazy val getJourney:         StepCollection = journey
  @transient lazy val getConsumables:     List[Consumable] = consumables
  @transient lazy val getBodyParts:       PlayerBodyCollection = bodyParts
  @transient lazy val getBaseAttributes:  AttributeCollection = baseAttributes
  @transient lazy val getRace:            Race = race
  @transient lazy private val equipments: List[Equipment] = bodyParts.equipments
  @transient lazy private val equipmentModifier: TransformationCollection = {
    equipments
      .map(_.modifiers)
      .safeReduce(_ ++ _)(TransformationCollection.Empty)
  }

  @transient lazy val currentAttributes: IO[Err, AttributeCollection] =
    baseAttributes << equipmentModifier

  def isWearing(e: Equipment): Boolean = equipments.exists(_ === e)

  def toStatus: String = ""

  def copy(
    name:           String = this.name,
    journey:        StepCollection = this.journey,
    consumables:    List[Consumable] = this.consumables,
    bodyParts:      PlayerBodyCollection = this.bodyParts,
    baseAttributes: AttributeCollection = this.baseAttributes,
    race:           Race = this.race
  ): Player = {
    new Player(name, journey, consumables, bodyParts, baseAttributes, race)
  }

  /**
    * for testing
    */
  private[Adventure] def equipWild(equipment: Equipment): IO[Err, Player] = {
    bodyParts
      .equip(equipment)
      .map(newBodyPart => copy(bodyParts = newBodyPart))
  }

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
          p.getName,
          p.getJourney.encoded,
          p.getConsumables.map(_.encoded),
          p.getBodyParts.encoded,
          p.getBaseAttributes.encoded,
          p.getRace.encoded
        )
      }

  override def encoded: Json = this.asJson

  override def toString: String = encoded.noSpaces
}

sealed trait PlayerTrait {
  def encoded: Json
}

object PlayerBuilder {
  val NameQuestion: String = "What is your name?"
  val RaceQuestion: String = "What race are you part of ?"

  val Empty: Player = PlayerWithName("")
    .selectRace(Races.Void)

  case class PlayerWithName(name: String) extends PlayerTrait {
    def selectRace(race: Race): Player = {
      new Player(
        name = name.trim,
        journey = StepCollection.Empty,
        consumables = Nil,
        bodyParts = race.getBodyParts.toPlayerBodyCollection,
        baseAttributes = race.getBaseAttributes,
        race = race
      )
    }

    def selectRace(availableRaces: List[Race])(race: String): IO[Err, Player] = {
      availableRaces.find(_.getLowerCaseName == race.toLowerCase) match {
        case Some(r) => IO.sync(selectRace(r))
        case None =>
          ErrorIO(
            "Not found race",
            s"Could not found '$race' among: " +
              s"${availableRaces.map(_.getCapitalizeName).mkString(", ")}"
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
