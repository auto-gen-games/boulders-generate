package org.drsimonmiles.itapprox

import scala.util.Random.shuffle

final case class Choice[State] (options: List[Decision[State]], preferFirst: Boolean) {
  def shuffled: List[Decision[State]] = if (preferFirst) options else shuffle (options)
}

object Choice {
  def apply[State] (options: Decision[State]*): Choice[State] =
    new Choice (options.toList, preferFirst = false)

  def preferential[State] (options: Decision[State]*): Choice[State] =
    new Choice (options.toList, preferFirst = true)
}
