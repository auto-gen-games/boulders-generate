package org.drsimonmiles.rocks

import org.drsimonmiles.itapprox.Decision
import org.drsimonmiles.rocks.Puzzle._

case class SetBoulder (x: Int, y: Int, present: Boolean) extends Decision[Puzzle] {
  def enact (puzzle: Puzzle) = setBoulder (puzzle, x, y, present)
  def not = SetBoulder (x, y, !present)
}

case class SetFloor (x: Int, y: Int, present: Boolean) extends Decision[Puzzle] {
  def enact (puzzle: Puzzle) = setFloor (puzzle, x, y, present)
  def not = SetFloor (x, y, !present)
}

case class SetLeftWall (x: Int, y: Int, present: Boolean) extends Decision[Puzzle] {
  def enact (puzzle: Puzzle) = setLeftWall (puzzle, x, y, present)
  def not = SetLeftWall (x, y, !present)
}
