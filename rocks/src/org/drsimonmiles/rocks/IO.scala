package org.drsimonmiles.rocks

import java.io.{File, FileWriter, PrintWriter}
import scala.io.Source

object IO {
  def addToFile (puzzlesFile: File, puzzle: Puzzle): Unit = this.synchronized {
    val out = new PrintWriter (new FileWriter (puzzlesFile, puzzlesFile.exists))
    out.println (Puzzle.toCode (puzzle))
    out.close ()
  }

  def load (puzzlesFile: File): List[Puzzle] = {
    val source = Source.fromFile (puzzlesFile)
    val puzzles = source.getLines.map (Puzzle.fromCode).toList.flatten
    source.close ()
    puzzles
  }

  def saveMetrics (metricsFile: String, data: List[(Int, Int, Int, Int)]): Unit = {
    val out = new PrintWriter (new FileWriter (metricsFile))
    out.println ("Index,Length,Breadth,Weaving")
    data.foreach (v => out.println (v._1 + "," + v._2 + "," + v._3 + "," + v._4))
    out.close ()
  }
}
