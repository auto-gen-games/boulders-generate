package org.drsimonmiles.rocks

import java.io.{File, FileWriter, PrintWriter}

import org.drsimonmiles.rocks.IO.addToFile
import org.drsimonmiles.rocks.Metrics.challenge
import org.drsimonmiles.rocks.PuzzleCreation.createPuzzle

object Main extends App {
  val configFile = "command.json"
  val configuration = Configuration.fromSource (configFile) match {
    case Left (error) =>
      System.err.println (s"Could not load configuration file $configFile")
      System.err.println (error.getMessage)
      System.exit (1)
      null
    case Right (data) => data
  }

  configuration.commands.foreach {
    case command @ CreateCommand (width, height, number, _, puzzlesDirectory, _, _, _) =>
      println (s"Generating for $width x $height")
      var created = 0
      val puzzlesFileName = s"puzzles-$width-by-$height.txt"
      val puzzlesFile = new File (new File (puzzlesDirectory), puzzlesFileName)
      var minChallenge = 0
      while (created < number) {
        print (".")
        val newPuzzle = createPuzzle (width, height, width * height * 10, width * height * 5000L, minChallenge)(command)
        if (newPuzzle.isDefined) {
          this.synchronized (print ("+"))
          minChallenge = minChallenge.max (Metrics.challenge (newPuzzle.get._1))
        }
        newPuzzle.foreach (puzzleSolution => addToFile (puzzlesFile, puzzleSolution._1))
        created += newPuzzle.size
      }
    case SolveCommand (puzzlesDirectory, inputFile, outputFile, _, _, _) =>
      processPuzzles (puzzlesDirectory, inputFile, outputFile){
        _.map (puzzle => Solve.toString (Solve.solve (Game (puzzle))(() => false))).mkString ("\n")
      }
    case command @ ShowCommand (puzzlesDirectory, inputFile, outputFile, _, _, _) =>
      processPuzzles (puzzlesDirectory, inputFile, outputFile){
        _.map (p => Puzzle.toString (p)(command)).mkString ("\n\n")
      }
    case RateCommand (puzzlesDirectory, inputFile, outputFile, _, _) =>
      processPuzzles (puzzlesDirectory, inputFile, outputFile){
        _.map (level => (level, challenge (level))).map (_._2).mkString ("\n")
      }
    case SortCommand (puzzlesDirectory, inputFile, outputFile, _, _) =>
      processPuzzles (puzzlesDirectory, inputFile, outputFile){
        _.map (level => (level, challenge (level))).sortBy (_._2).map (s => Puzzle.toCode (s._1)).mkString ("\n")
      }
  }

  def processPuzzles (directory: String, input: String, output: String)(generate: Seq[Puzzle] => String): Unit = {
    val puzzles = IO.load (new File (directory, input))
    val result = generate (puzzles)
    val out = new PrintWriter (new FileWriter (new File (directory, output)))
    out.println (result)
    out.close ()
  }
}
