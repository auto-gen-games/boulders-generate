package org.drsimonmiles.rocks

import java.io.File
import org.drsimonmiles.rocks.IO.addToFile
import org.drsimonmiles.rocks.PuzzleCreation.createPuzzle
import org.drsimonmiles.util.{Logger, Measure}

/** Main application for creating puzzles */
object Create extends App {
  println ("Running")

  val configFile = "command.json"
  implicit val configuration = Configuration.fromSource (configFile) match {
    case Left (error) =>
      System.err.println (s"Could not load configuration file $configFile")
      System.err.println (error.getMessage)
      System.exit (1)
      null
    case Right (data) => data
  }
  import configuration._

  if (args.length > 0 && args(0) == "sort") {
    if (args.length > 2)
      SortByDifficulty.sort (args (1), args (2))
    System.exit (0)
  }

  if (args.length > 0 && args(0) == "rate") {
    if (args.length > 1)
      SortByDifficulty.rate (args (1))
    System.exit (0)
  }

  if (args.length > 0 && args(0) == "show") {
    val puzzlesFile = new File (args (1))
    println (IO.load (puzzlesFile).map (p => Puzzle.toString (p)).mkString ("\n\n"))
    System.exit (0)
  }

  if (args.length > 0 && args(0) == "solve") {
    val puzzlesFile = new File (args (1))
    val puzzles = IO.load (puzzlesFile)
    println (puzzles.map (puzzle => Solve.toString(Solve.solve(Game (puzzle))(() => false))).mkString("\n"))
    System.exit (0)
  }

  if (args.length > 0 && args(0) == "trial") {
    val puzzle = createPuzzle (startWidth, startHeight, startWidth * startHeight * 10, startWidth * startHeight * 2000l, 0)
    if (puzzle.isDefined) println (Puzzle.toString (puzzle.get._1)) else println ("No puzzle found")
    Logger.saveLog ()
    println (Measure.toString)
    System.exit (0)
  }

  // Keep generating puzzles, using all the cores in parallel
  var height = startHeight
  var minLengthForHeight = 0
  while (true) {
    var minLengthForArea = minLengthForHeight
    minLengthForHeight = minLengthForHeight.max ((for (width <- startWidth to height * 2) yield {
      println (s"Generating for $width x $height (min length: $minLengthForArea)")
      var created = 0
      val puzzlesFileName = s"puzzles-$width-by-$height.txt"
      val puzzlesFile = new File (new File (puzzlesDirectory), puzzlesFileName)
      var nextMinLength = minLengthForArea
      while (created < numberToCreatePerSize) {
          print (".")
          val newPuzzle = createPuzzle (width, height, width * height * 10, width * height * 5000l, minLengthForArea)
          if (newPuzzle.isDefined) {
            this.synchronized (print ("+"))
            nextMinLength = nextMinLength.max (Metrics.length (newPuzzle.get._2))
          }
          newPuzzle.foreach (puzzleSolution => addToFile (puzzlesFile, puzzleSolution._1))
          created += newPuzzle.size
        }
      minLengthForArea = nextMinLength
      minLengthForArea
    }).min)
    height += 1
  }
}
