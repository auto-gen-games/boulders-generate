package org.drsimonmiles.util

object Matrix {
  /** Update an element of a 2D table made of Lists */
  def updated[E] (matrix: List[List[E]], index1: Int, index2: Int, value: E): List[List[E]] =
    matrix.updated (index1, matrix (index1).updated (index2, value))
}
