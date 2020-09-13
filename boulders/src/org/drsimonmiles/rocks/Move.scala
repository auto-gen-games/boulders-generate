package org.drsimonmiles.rocks

sealed trait Move
case object WalkLeft extends Move
case object WalkRight extends Move
case object JumpLeft extends Move
case object JumpRight extends Move
case object Jump extends Move
case object Fall extends Move

object Move {
  val allMoves = Seq (WalkLeft, WalkRight, JumpLeft, JumpRight, Jump, Fall)

  def toString (move: Move): String =
    move match {
      case WalkLeft => "L"
      case WalkRight => "R"
      case JumpLeft => "UL"
      case JumpRight => "UR"
      case Jump => "U"
      case _ => ""
    }
}