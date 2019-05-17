package org.drsimonmiles.itapprox

import scala.util.Random.{nextDouble, shuffle}

/**
  * A choice between multiple decisions on how to alter a given state.
  * @param options The decisions to choose between
  * @param preferFirst Whether the options will retain their order with first decision first even when shuffled.
  * @tparam State The type of state over which a decision is made
  */
final case class Choice[State] (options: List[Decision[State]], preferFirst: Boolean) {
  /** Returns the list of options in this choice randomised unless this option prefers the order to remain static. */
  def shuffled: List[Decision[State]] = if (preferFirst) options else shuffle (options)
}

/** Functions to create a choice between decisions. */
object Choice {
  /** Creates a choice between the given options with no constraint to retain the order given on creation. */
  def apply[State] (options: Decision[State]*): Choice[State] =
    new Choice (options.toList, preferFirst = false)

  /** Creates a choice between the given options constrained to retain the order given on creation. */
  def preferential[State] (options: Decision[State]*): Choice[State] =
    new Choice (options.toList, preferFirst = true)

  /** Creates a choice between the given options, where
    * bias is the increase in likelihood of the first option listed remaining first after shuffling: 0.0 for no bias,
    * 1.0 for certain that first will remain first */
  def biased[State] (bias: Double, options: Decision[State]*): Choice[State] =
    new Choice (options.toList, preferFirst = nextDouble < bias)
}
