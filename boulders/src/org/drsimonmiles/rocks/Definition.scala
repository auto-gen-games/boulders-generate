package org.drsimonmiles.rocks

trait Definition {
  val name: String
  val moves: Vector[Move]
  def acceptablyHard (puzzle: Puzzle, solution: List[Move], context: Int): Boolean
}

object Definition {
  val baseGameDefinition: Definition = new Definition {
    val name = "base"
    val moves = Vector (WalkLeft, WalkRight, JumpLeft, JumpRight, Jump, Fall)
    def acceptablyHard (puzzle: Puzzle, solution: List[Move], pastLength: Int): Boolean =
      Metrics.challenge (puzzle)(this) >= pastLength
  }

  val flipGameDefinition: Definition = new Definition {
    val name = "flip"
    val moves = Vector (WalkLeft, WalkRight, JumpLeft, JumpRight, Jump, Fall, Flip)
    def acceptablyHard (puzzle: Puzzle, solution: List[Move], pastLength: Int): Boolean =
      solution.contains (Flip) &&
        Solve.solve (Game (puzzle))(baseGameDefinition).isEmpty &&
        Metrics.challenge (puzzle)(this) >= pastLength
  }

  def fromName (name: String): Option[Definition] =
    name match {
      case "base" => Some (baseGameDefinition)
      case "flip" => Some (flipGameDefinition)
      case _ => None
    }
}
