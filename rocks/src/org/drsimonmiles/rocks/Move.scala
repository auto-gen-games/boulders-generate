package org.drsimonmiles.rocks

sealed trait Move
case object Left extends Move
case object Right extends Move
case object JumpLeft extends Move
case object JumpRight extends Move
case object Jump extends Move
case object Fall extends Move

object Move {
  val allMoves = Seq (Left, Right, JumpLeft, JumpRight, Jump, Fall)
}