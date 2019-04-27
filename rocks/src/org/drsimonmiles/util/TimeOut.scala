package org.drsimonmiles.util

import java.lang.System.currentTimeMillis

object TimeOut {
  /** Creates a function returning true if the time limit, starting from now, has been exceeded */
  def timeOutFromNow (timeLimit: Long): () => Boolean = {
    val stopAt = currentTimeMillis + timeLimit
    () => System.currentTimeMillis >= stopAt
  }
}
