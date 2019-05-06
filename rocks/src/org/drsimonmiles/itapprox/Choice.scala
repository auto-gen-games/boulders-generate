package org.drsimonmiles.itapprox

import scala.util.Random.{nextDouble, shuffle}

final case class Choice[State] (options: List[Decision[State]], preferFirst: Boolean) {
  def shuffled: List[Decision[State]] = if (preferFirst) options else shuffle (options)
}

object Choice {
  def apply[State] (options: Decision[State]*): Choice[State] =
    new Choice (options.toList, preferFirst = false)

  def preferential[State] (options: Decision[State]*): Choice[State] =
    new Choice (options.toList, preferFirst = true)

  /** bias is the increase in likelihood of the first option listed remaining first after shuffling: 0.0 for no bias,
    * 1.0 for certain that first will remain first */
  def biased[State] (bias: Double, options: Decision[State]*): Choice[State] =
    new Choice (options.toList, preferFirst = nextDouble < bias)
}
