package org.drsimonmiles.rocks

import java.io.{File, FileWriter, PrintWriter}

import org.drsimonmiles.rocks.IO.addToFile
import org.drsimonmiles.rocks.Metrics.challenge
import org.drsimonmiles.rocks.PuzzleCreation.createPuzzle

object Main extends App {
  val configFile = if (args.length > 0) args (0) else "command.json"
  val configuration = Configuration.fromSource (configFile) match {
    case Left (error) =>
      System.err.println (s"Could not load configuration file $configFile")
      System.err.println (error.getMessage)
      System.exit (1)
      null
    case Right (data) => data
  }

  var minChallenge = 0
  configuration.commands.foreach {
    case command @ CreateCommand (_, _, _, _, _, _, _, _, _, _) =>
      import command._
      println (s"Generating for $width x $height")
      var created = 0
      val puzzlesFileName = s"puzzles-$width-by-$height-${definition.name}.txt"
      val puzzlesFile = new File (new File (puzzlesDirectory), puzzlesFileName)
      while (created < number) {
        print (".")
        val newPuzzle = createPuzzle (width, height, width * height * 10, width * height * 5000L, (minChallenge * challengeImprovement).toInt)(command)
        if (newPuzzle.isDefined) {
          this.synchronized (print ("+"))
          minChallenge = minChallenge.max (Metrics.challenge (newPuzzle.get._1)(definition))
        }
        newPuzzle.foreach (puzzleSolution => addToFile (puzzlesFile, puzzleSolution._1))
        created += newPuzzle.size
      }
    case SolveCommand (definition, puzzlesDirectory, inputFile, outputFile, _, _) =>
      processPuzzles (puzzlesDirectory, inputFile, outputFile){
        _.map (puzzle => Solve.toString (Solve.solve (Game (puzzle))(definition))).mkString ("\n")
      }
    case command @ ShowCommand (definition, puzzlesDirectory, inputFile, outputFile, _, _) =>
      processPuzzles (puzzlesDirectory, inputFile, outputFile){
        _.map (p => Puzzle.toString (p)(command)).mkString ("\n\n")
      }
    case RateCommand (definition, puzzlesDirectory, inputFile, outputFile, _, _) =>
      processPuzzles (puzzlesDirectory, inputFile, outputFile){
        _.map (level => (level, challenge (level)(definition))).map (_._2).mkString ("\n")
      }
    case SortCommand (definition, puzzlesDirectory, inputFile, outputFile, _, _) =>
      processPuzzles (puzzlesDirectory, inputFile, outputFile){
        _.map (level => (level, challenge (level)(definition))).sortBy (_._2).map (s => Puzzle.toCode (s._1)).mkString ("\n")
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
