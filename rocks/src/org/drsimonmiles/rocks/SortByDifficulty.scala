package org.drsimonmiles.rocks

import org.drsimonmiles.rocks.IO.{addToFile, load}
import org.drsimonmiles.rocks.Metrics.breadth
import java.io.File

object SortByDifficulty extends App {
  sort (args (0), args (1))

  def sort (levelsFile: String, sortedFile: String) = {
    val input = new File (levelsFile)
    val output = new File (sortedFile)
    val levels = load (input)
    val rated = levels.map (level => (level, breadth (level)))
    val sorted = rated.sortBy (_._2).map (_._1)

    sorted.foreach (level => addToFile (output, level))
  }
}
