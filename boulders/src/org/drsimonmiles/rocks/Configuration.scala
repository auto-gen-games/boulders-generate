package org.drsimonmiles.rocks

import io.circe._
import io.circe.parser._
import scala.io.Source

abstract class Command {
  val measuring: Boolean
  val logging: Boolean
}
abstract class SolvingCommand extends Command {
  val maxSolveTime: Long
}
case class Configuration (commands: Vector[Command])

case class CreateCommand (
  // The width of the puzzles created
  width: Int,
  // The height of the puzzles created
  height: Int,
  // Number of puzzles created for this width x height
  number: Int,
  // When deciding whether to place a boulder in refining a puzzle, this is the bias against doing so
  // (to avoid puzzles filled with boulders): 0.0 means no bias, 1.0 means always first try not placing a boulder
  biasAgainstBoulders: Double,
  // Directory where puzzles are stored
  puzzlesDirectory: String,
  // When time-out occurs
  maxSolveTime: Long,
  // Whether performance measuring is turned on
  measuring: Boolean,
  // Whether logging is turned on
  logging: Boolean
) extends Command

case class SolveCommand (
  // Directory where puzzles are stored
  puzzlesDirectory: String,
  // File from which to load puzzles to solve
  inputFile: String,
  // File to which to write solutions
  outputFile: String,
  // When time-out occurs
  maxSolveTime: Long,
  // Whether performance measuring is turned on
  measuring: Boolean,
  // Whether logging is turned on
  logging: Boolean
) extends SolvingCommand

case class ShowCommand (
  // Directory where puzzles are stored
  puzzlesDirectory: String,
  // File from which to load puzzles to solve
  inputFile: String,
  // File to which to write solutions
  outputFile: String,
  // When time-out occurs
  maxSolveTime: Long,
  // Whether performance measuring is turned on
  measuring: Boolean,
  // Whether logging is turned on
  logging: Boolean
) extends SolvingCommand

case class RateCommand (
  // Directory where puzzles are stored
  puzzlesDirectory: String,
  // File from which to load puzzles to solve
  inputFile: String,
  // File to which to write solutions
  outputFile: String,
  // Whether performance measuring is turned on
  measuring: Boolean,
  // Whether logging is turned on
  logging: Boolean
) extends Command

case class SortCommand (
  // Directory where puzzles are stored
  puzzlesDirectory: String,
  // File from which to load puzzles to solve
  inputFile: String,
  // File to which to write solutions
  outputFile: String,
  // Whether performance measuring is turned on
  measuring: Boolean,
  // Whether logging is turned on
  logging: Boolean
) extends Command

object Configuration {
  case class Defaults (biasAgainstBoulders: Double, puzzlesDirectory: String, maxSolveTime: Long,
                       measuring: Boolean, logging: Boolean)

  def fromSource (jsonFile: String): Either[Throwable, Configuration] = {
    val json = Source.fromResource (jsonFile).getLines.mkString ("\n")
    parse (json) match {
      case Left (error) => Left (error)
      case Right (code) =>
        val cursor = code.hcursor
        val defaults = loadDefaults (cursor.downField("defaults"))
        implicit val commandDecoder: Decoder[Command] = loadCommand (defaults)
        cursor.get[Vector[Command]]("commands").map (Configuration (_))
    }
  }

  def loadDefaults (cursor: ACursor): Defaults =
    Defaults (
      biasAgainstBoulders = cursor.get[Double]("biasAgainstBoulders").getOrElse (0.5),
      puzzlesDirectory = cursor.get[String]("puzzlesDirectory").getOrElse ("puzzles"),
      maxSolveTime = cursor.get[Long]("maxSolveTime").getOrElse (10000L),
      measuring = cursor.get[Boolean]("measuring").getOrElse (false),
      logging = cursor.get[Boolean]("logging").getOrElse (false)
    )

  def loadCommand (defaults: Defaults): Decoder[Command] = (cursor: HCursor) => {
    cursor.get[String]("command") match {
      case Left (error) => Left (error)
      case Right ("create") => loadCreate (cursor, defaults)
      case Right (x) => Left (DecodingFailure (s"Unrecognised command $x", List.empty))
    }
  }

  def loadCreate (cursor: ACursor, defaults: Defaults): Either[DecodingFailure, CreateCommand] =
    for {
      width <- cursor.get[Int]("width")
      height <- cursor.get[Int]("height")
      number <- cursor.get[Int]("number")
    } yield CreateCommand (
      width = width,
      height = height,
      number = number,
      biasAgainstBoulders = cursor.get[Double]("biasAgainstBoulders").getOrElse (defaults.biasAgainstBoulders),
      puzzlesDirectory = cursor.get[String]("puzzlesDirectory").getOrElse (defaults.puzzlesDirectory),
      maxSolveTime = cursor.get[Long]("maxSolveTime").getOrElse (defaults.maxSolveTime),
      measuring = cursor.get[Boolean]("measuring").getOrElse (defaults.measuring),
      logging = cursor.get[Boolean]("logging").getOrElse (defaults.logging))
}