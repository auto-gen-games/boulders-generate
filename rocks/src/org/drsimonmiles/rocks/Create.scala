package org.drsimonmiles.rocks

import org.drsimonmiles.rocks.Configuration.{numberOfCores, numberToCreate, puzzlesFile}
import org.drsimonmiles.rocks.IO.addToFile
import org.drsimonmiles.rocks.PuzzleCreation.createPuzzle
import org.drsimonmiles.util.{Logger, Measure}

object Create extends App {
  println ("Running")

  if (args.length > 0 && args(0) == "show") {
    println (IO.load (puzzlesFile).map (p => Puzzle.toString (p)).mkString ("\n\n"))
    System.exit (0)
  }

  if (args.length > 0 && args(0) == "trial") {
    val puzzle = createPuzzle ()
    if (puzzle.isDefined) println (Puzzle.toString (puzzle.get)) else println ("No puzzle found")
    Logger.saveLog ()
    println (Measure.toString)
    System.exit (0)
  }

  // Keep generating puzzles, using all the cores in parallel
  var created = 0
  while (created < numberToCreate)
    created += (1 to numberOfCores).par.map { _ =>
      this.synchronized (print ("."))
      val newPuzzle: Option[Puzzle] = createPuzzle ()
      if (newPuzzle.isDefined) this.synchronized (print ("+"))
      this.synchronized (newPuzzle.foreach (addToFile (puzzlesFile, _)))
      newPuzzle.size
    }.sum
}
