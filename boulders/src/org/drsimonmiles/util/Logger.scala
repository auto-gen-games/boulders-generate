package org.drsimonmiles.util

import java.io.{File, FileWriter, PrintWriter}
import org.drsimonmiles.rocks.{Configuration, Metrics, Move, Puzzle}

object Logger {
  private var toLog = Vector[String] ()

  def show[A] (value: A): A = { println (value); value }

  def log[A] (value: A)(implicit config: Configuration): A = {
    if (config.logging) toLog = s"${value.toString}\n" +: toLog
    value
  }

  def saveLog ()(implicit config: Configuration): Unit = if (config.logging) {
    val logFile = new File ("trace.txt")
    val out = new PrintWriter (new FileWriter (logFile))
    toLog.reverse.foreach (out.println)
    out.close ()
  }

  def trace[P] (puzzle: P)(implicit config: Configuration): P = {
    if (config.logging) toLog = (toDetail (puzzle.asInstanceOf[Puzzle]) + "\n") +: toLog
    puzzle
  }

  def probe[P, A] (puzzle: P, solution: Option[A])(implicit config: Configuration): Option[A] = {
    if (config.logging)
      if (solution.isDefined) {
        val length = Metrics.length (solution.get.asInstanceOf[List[Move]])
        val weaving = Metrics.weaving (puzzle.asInstanceOf[Puzzle], solution.get.asInstanceOf[List[Move]])
        toLog = s"solved with length $length, weaving $weaving\n" +: toLog
      } else toLog = "not solved\n" +: toLog
    solution
  }

  def record (accepted: Boolean)(implicit config: Configuration): Boolean = {
    if (config.logging) toLog = s"Accepted? $accepted\n" +: toLog
    accepted
  }

  def toDetail (puzzle: Puzzle): String = {
    import puzzle._
    val outer = "+" + (for (_ <- 0 until width) yield "-+").mkString
    def within (y: Int) = "|" + (for (x <- 0 until width) yield {
      (if (hasBoulder (x, y)) "O" else if (hasMan (x, y)) "M" else if (hasExit (x, y)) "X" else if (hasStar (x, y)) "*" else if (knowBoulder (x, y)) " " else "?") +
        (if (hasLeftWall (x + 1, y)) "|" else if (knowLeftWall (x + 1, y)) " " else "{")
    }).mkString
    def under (y: Int) = "+" + (for (x <- 0 until width) yield {
      (if (hasFloor (x, y)) "-" else if (knowFloor (x, y)) " " else "~") + "+"}
      ).mkString
    outer + "\n" + (for (y <- 0 until height) yield {within (y) + "\n" + under (y) + "\n"}).mkString
  }
}
