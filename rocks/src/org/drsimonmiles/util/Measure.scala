package org.drsimonmiles.util

import java.lang.System.currentTimeMillis

import org.drsimonmiles.rocks.Configuration

object Measure {
  // Each block is measured by minimum time spent, maximum time spent, total time spent, number of calls to block
  private val times = scala.collection.mutable.Map[String, (Long, Long, Long, Long)] ()

  def measure[T] (name: String, f: => T) = if (Configuration.measuring) {
    val start = currentTimeMillis
    val result = f
    val time = currentTimeMillis - start
    val prev = times.getOrElse (name, (0l, 0l, 0l, 0l))
    val min = if (time < prev._1) time else prev._1
    val max = if (time > prev._2) time else prev._2
    times (name) = (min, max, prev._3 + time, prev._4 + 1)
    result
  } else f

  // Display each block in order: minimum - average - maximum - total - occurrences
  override def toString =
    times.keys.toVector.sorted.map { key =>
      key + " [" + times (key)._1 + " (min) - " + (times (key)._3 / times (key)._4) + " (avg) - " + times (key)._2 + " (max) - " + times (key)._3 + " (tot) - " + times (key)._4 + " (count)]"
    }.mkString ("\n")
}
