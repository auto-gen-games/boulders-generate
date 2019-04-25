package org.drsimonmiles.util

class GeneratedMap[Key, Value] (generate: Key => Value) {
  private var buffered = Map [Key, Value] ()

  def apply (key: Key): Value =
    getOrGenerate (key, key)

  def getOrGenerate (lookupKey: Key, generateSeed: Key): Value =
    buffered.get (lookupKey) match {
      case Some (found) => found
      case None =>
        val found = Measure.measure ("generate", generate (generateSeed))
        buffered = buffered + (lookupKey -> found)
        found
    }
}
