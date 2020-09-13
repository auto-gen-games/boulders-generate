package org.drsimonmiles.rocks

import io.circe._
import io.circe.parser._

case class Configuration (
  // The command to perform
  command: String,
  // The smallest width puzzle
  startWidth: Int,
  // The smallest height puzzle
  startHeight: Int,
  // Number of puzzles created for each width x height
  numberToCreatePerSize: Int,
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
)

object Configuration {
  def fromSource (jsonFile: String): Either[Throwable, Configuration] = {
    val json = scala.io.Source.fromResource (jsonFile).getLines.mkString ("\n")
    for {
      parsed <- parse (json)
      decoded <- decoder (parsed.hcursor)
    } yield decoded
  }

  val decoder: Decoder[Configuration] = (hCursor: HCursor) =>
    for {
      command <- hCursor.get[String]("command")
      startWidth <- hCursor.get[Int]("startWidth")
      startHeight <- hCursor.get[Int]("startHeight")
      numberToCreatePerSize <- hCursor.get[Int]("numberToCreatePerSize")
      biasAgainstBoulders <- hCursor.get[Double]("biasAgainstBoulders")
      puzzlesDirectory <- hCursor.get[String]("puzzlesDirectory")
      maxSolveTime <- hCursor.get[Int]("maxSolveTime")
      measuring <- hCursor.get[Boolean]("measuring")
      logging <- hCursor.get[Boolean]("logging")
    } yield Configuration (startWidth = startWidth,
        command = command,
        startHeight = startHeight,
        numberToCreatePerSize = numberToCreatePerSize,
        biasAgainstBoulders = biasAgainstBoulders,
        puzzlesDirectory = puzzlesDirectory,
        maxSolveTime = maxSolveTime,
        measuring = measuring,
        logging = logging)
}