package org.drsimonmiles.util

object Matrix {
  def updated[E] (matrix: List[List[E]], index1: Int, index2: Int, value: E): List[List[E]] =
    matrix.updated (index1, matrix (index1).updated (index2, value))
}
