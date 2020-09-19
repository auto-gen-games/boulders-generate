package org.drsimonmiles.rocks

import io.circe._
import io.circe.parser._
import org.drsimonmiles.rocks.Definition.baseGameDefinition

import scala.io.Source

trait Command {
  val definition: Definition
  val measuring: Boolean
  val logging: Boolean
}
case class Configuration (commands: Vector[Command])

case class CreateCommand (
                           // Definition of this puzzle type
                           definition: Definition,
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
                           // When time-out occurs in one creation attempt
                           maxAttemptTime: Long,
                         // What minimum challenge each puzzle must have as a proportion of the best so far
                         challengeImprovement: Double,
                           // Whether performance measuring is turned on
                           measuring: Boolean,
                           // Whether logging is turned on
                           logging: Boolean
) extends Command

case class SolveCommand (
  // Definition of this puzzle type
  definition: Definition,
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

case class ShowCommand (
                         // Definition of this puzzle type
                         definition: Definition,
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

case class RateCommand (
                         // Definition of this puzzle type
                         definition: Definition,
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
                         // Definition of this puzzle type
                         definition: Definition,
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
  case class Defaults (definition: Definition, biasAgainstBoulders: Double, puzzlesDirectory: String,
                       maxAttemptTime: Long, challengeImprovement: Double, measuring: Boolean, logging: Boolean)

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

  def loadDefinition (result: Decoder.Result[String]): Decoder.Result[Definition] =
    result.map (Definition.fromName) match {
      case Left (notFound) => Left (notFound)
      case Right (None) => Left (DecodingFailure ("Unknown game definition name", List.empty))
      case Right (Some (found)) => Right (found)
    }

  def loadDefaults (cursor: ACursor): Defaults =
    Defaults (
      definition = loadDefinition (cursor.get[String]("definition")).getOrElse (baseGameDefinition),
      biasAgainstBoulders = cursor.get[Double]("biasAgainstBoulders").getOrElse (0.5),
      puzzlesDirectory = cursor.get[String]("puzzlesDirectory").getOrElse ("puzzles"),
      maxAttemptTime = cursor.get[Long]("maxAttemptTime").getOrElse (10000L),
      challengeImprovement = cursor.get[Double]("challengeImprovement").getOrElse(0.8),
      measuring = cursor.get[Boolean]("measuring").getOrElse (false),
      logging = cursor.get[Boolean]("logging").getOrElse (false)
    )

  def loadCommand (defaults: Defaults): Decoder[Command] = (cursor: HCursor) => {
    cursor.get[String]("command") match {
      case Left (error) => Left (error)
      case Right ("create") => loadCreate (cursor, defaults)
      case Right ("solve") => loadSolve (cursor, defaults)
      case Right ("show") => loadShow (cursor, defaults)
      case Right ("rate") => loadRate (cursor, defaults)
      case Right ("sort") => loadSort (cursor, defaults)
      case Right (x) => Left (DecodingFailure (s"Unrecognised command $x", List.empty))
    }
  }

  def loadCreate (cursor: ACursor, defaults: Defaults): Either[DecodingFailure, CreateCommand] =
    for {
      width <- cursor.get[Int]("width")
      height <- cursor.get[Int]("height")
      number <- cursor.get[Int]("number")
    } yield CreateCommand (
      definition = loadDefinition (cursor.get[String]("definition")).getOrElse (defaults.definition),
      width = width,
      height = height,
      number = number,
      biasAgainstBoulders = cursor.get[Double]("biasAgainstBoulders").getOrElse (defaults.biasAgainstBoulders),
      puzzlesDirectory = cursor.get[String]("puzzlesDirectory").getOrElse (defaults.puzzlesDirectory),
      maxAttemptTime = cursor.get[Long]("maxAttemptTime").getOrElse (defaults.maxAttemptTime),
      challengeImprovement = cursor.get[Double]("challengeImprovement").getOrElse (defaults.challengeImprovement),
      measuring = cursor.get[Boolean]("measuring").getOrElse (defaults.measuring),
      logging = cursor.get[Boolean]("logging").getOrElse (defaults.logging))

  def loadSolve (cursor: ACursor, defaults: Defaults): Either[DecodingFailure, SolveCommand] =
    for {
      input <- cursor.get[String]("inputFile")
      output <- cursor.get[String]("outputFile")
    } yield SolveCommand (
      definition = loadDefinition (cursor.get[String]("definition")).getOrElse (defaults.definition),
      puzzlesDirectory = cursor.get[String]("puzzlesDirectory").getOrElse (defaults.puzzlesDirectory),
      inputFile = input,
      outputFile = output,
      measuring = cursor.get[Boolean]("measuring").getOrElse (defaults.measuring),
      logging = cursor.get[Boolean]("logging").getOrElse (defaults.logging))

  def loadShow (cursor: ACursor, defaults: Defaults): Either[DecodingFailure, ShowCommand] =
    for {
      input <- cursor.get[String]("inputFile")
      output <- cursor.get[String]("outputFile")
    } yield ShowCommand (
      definition = loadDefinition (cursor.get[String]("definition")).getOrElse (defaults.definition),
      puzzlesDirectory = cursor.get[String]("puzzlesDirectory").getOrElse (defaults.puzzlesDirectory),
      inputFile = input,
      outputFile = output,
      measuring = cursor.get[Boolean]("measuring").getOrElse (defaults.measuring),
      logging = cursor.get[Boolean]("logging").getOrElse (defaults.logging))

  def loadRate (cursor: ACursor, defaults: Defaults): Either[DecodingFailure, RateCommand] =
    for {
      input <- cursor.get[String]("inputFile")
      output <- cursor.get[String]("outputFile")
    } yield RateCommand (
      definition = loadDefinition (cursor.get[String]("definition")).getOrElse (defaults.definition),
      puzzlesDirectory = cursor.get[String]("puzzlesDirectory").getOrElse (defaults.puzzlesDirectory),
      inputFile = input,
      outputFile = output,
      measuring = cursor.get[Boolean]("measuring").getOrElse (defaults.measuring),
      logging = cursor.get[Boolean]("logging").getOrElse (defaults.logging))

  def loadSort (cursor: ACursor, defaults: Defaults): Either[DecodingFailure, SortCommand] =
    for {
      input <- cursor.get[String]("inputFile")
      output <- cursor.get[String]("outputFile")
    } yield SortCommand (
      definition = loadDefinition (cursor.get[String]("definition")).getOrElse (defaults.definition),
      puzzlesDirectory = cursor.get[String]("puzzlesDirectory").getOrElse (defaults.puzzlesDirectory),
      inputFile = input,
      outputFile = output,
      measuring = cursor.get[Boolean]("measuring").getOrElse (defaults.measuring),
      logging = cursor.get[Boolean]("logging").getOrElse (defaults.logging))
}