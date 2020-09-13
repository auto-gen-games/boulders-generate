package org.drsimonmiles.util

import org.drsimonmiles.rocks.Configuration

/**
  * A map where any value requested will be generated if it does not already exist.
  *
  * @param generate A function to generate a new value for the given key
  * @tparam Key The type of the keys
  * @tparam Value The type of the values
  */
class GeneratedMap[Key, Value] (generate: Key => Value) {
  private var buffered = Map [Key, Value] ()

  /** Return the value for the given key, generated from the key if not previously existent. */
  def apply (key: Key)(implicit config: Configuration): Value =
    getOrGenerate (key, key)

  /** Return the value for the given key or generate and retain a value if one does not exist, separating out the key
    * that the value is mapped from and the seed used for generation in case these are different. */
  def getOrGenerate (lookupKey: Key, generateSeed: Key)(implicit config: Configuration): Value =
    buffered.get (lookupKey) match {
      case Some (found) => found
      case None =>
        val found = Measure.measure ("generate", generate (generateSeed))
        buffered = buffered + (lookupKey -> found)
        found
    }
}
