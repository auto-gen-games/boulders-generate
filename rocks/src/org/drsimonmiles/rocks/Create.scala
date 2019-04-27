package org.drsimonmiles.rocks

import java.io.File

import org.drsimonmiles.rocks.Configuration._
import org.drsimonmiles.rocks.IO.addToFile
import org.drsimonmiles.rocks.PuzzleCreation.createPuzzle
import org.drsimonmiles.util.{Logger, Measure}

object Create extends App {
  println ("Running")

  if (args.length > 0 && args(0) == "show") {
    val puzzlesFile = new File (args (1))
    println (IO.load (puzzlesFile).map (p => Puzzle.toString (p)).mkString ("\n\n"))
    System.exit (0)
  }

  if (args.length > 0 && args(0) == "trial") {
    Configuration.logging = true
    Configuration.measuring = true
    val puzzle = createPuzzle (startWidth, startHeight, startWidth * startHeight * 10, startWidth * startHeight * 2000l)
    if (puzzle.isDefined) println (Puzzle.toString (puzzle.get)) else println ("No puzzle found")
    Logger.saveLog ()
    println (Measure.toString)
    System.exit (0)
  }

  // Keep generating puzzles, using all the cores in parallel
  var height = startHeight
  while (!appTerminated) {
    for (width <- startWidth until height * 2) {
      var created = 0
      val puzzlesFileName = s"puzzles-$width-by-$height.txt"
      val puzzlesFile = new File (new File (puzzlesDirectory), puzzlesFileName)
      while (created < numberToCreatePerSize)
        created += (1 to numberOfCores).par.map { _ =>
          this.synchronized (print ("."))
          val newPuzzle: Option[Puzzle] = createPuzzle (width, height, width * height * 10, width * height * 2000l)
          if (newPuzzle.isDefined) this.synchronized (print ("+"))
          this.synchronized (newPuzzle.foreach (addToFile (puzzlesFile, _)))
          newPuzzle.size
        }.sum
    }
    height += 1
  }
}
