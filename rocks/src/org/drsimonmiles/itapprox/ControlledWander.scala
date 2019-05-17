package org.drsimonmiles.itapprox

import org.drsimonmiles.util.Measure

object ControlledWander {
  /**
    * Wanders through decision paths to try to find the best acceptable complete state, until termination criteria are met,
    * where completeness means that no compulsory decisions are left to be made on the state and acceptability is given by
    * a supplied function.
    *
    * @param initialState The initial state
    * @param initialChoices The decisions that must be made from the initial state
    * @param terminate Returns true if a timeout has been reached
    * @param followOn Returns the choices that must be made following the given decision on the given state before it can be complete
    * @param acceptable Returns true if the given state is an acceptable one to explore or should be ignored
    * @param improve Returns a non-compulsory choice on a given complete state that could help improve on that state
    * @param best Returns whichever of the pair of states is better
    * @param looksHopeless Returns true if exploring the given path looks unlikely to find good results
    * @return The best complete state found, if any, by timing out or running out of paths to follow
    */
  def wander[State] (initialState: State, initialChoices: List[Choice[State]])
                    (terminate: () => Boolean)
                    (followOn: (Decision[State], State) => List[Choice[State]])
                    (acceptable: State => Boolean)
                    (improve: State => Option[Choice[State]])
                    (best: (State, State) => State)
                    (looksHopeless: List[State] => Boolean): Option[State] = {
    var steps = 0

    /** Holds a position on a decision path wander, with the current state reached, the choices to be made, and the
      * prior states on the path (ordered from most recent first). */
    case class Position (state: State, choices: List[Choice[State]], prior: List[State])

    /** Returns the states reached by enacting each decision in a choice, where that is a valid setting, randomly ordered */
    def nextSteps (choice: Choice[State], oldState: Position, otherChoices: List[Choice[State]]): List[Position] =
      for (decision <- choice.shuffled; result <- decision.enact (oldState.state); consequences = org.drsimonmiles.util.Logger.log (followOn (decision, result))) yield
        Position (result, consequences ::: otherChoices, result :: oldState.prior)

    /** Wanders from given unexplored generation states, each with a decision stack, keeping the best found state */
    def wanderFrom (unexplored: List[Position], bestSoFar: Option[State]): Option[State] = unexplored match {
      case state :: remainder =>
        steps += 1
        // If the new state is acceptable and there are no decisions that need to be made to complete it,
        // test to see whether it's the best found so far, record if so
        val newBest =
          if (state.choices.isEmpty && Measure.measure ("CW.acceptable", acceptable (state.state)))
            bestSoFar.map (best (_, state.state)).orElse (Some (state.state))
          else bestSoFar
        // If the time is up, return the best solution found
        if (terminate ()) newBest
        else
          // If the decision path still looks worth exploring, make some choices, else try other unexplored paths
          if (!looksHopeless (state.prior))
            state.choices match {
              // Where there is a next choice required to be made, add each possible decision as a new state to be explored
              case choice :: rest =>
                org.drsimonmiles.util.Logger.log (s"Choice: $choice")
                wanderFrom (nextSteps (choice, state, rest) ::: remainder, newBest)
              // Where there are no more compulsory decisions, request choices to improve the current state and recurse
              case Nil =>
                wanderFrom (improve (state.state).map (choice => Position (state.state, List (choice), state.prior)).toList ++ remainder, newBest)
            }
        else
          wanderFrom (remainder, newBest)
      // If no decision paths are left to try, return the best solution found
      case Nil => bestSoFar
    }

    wanderFrom (List (Position (initialState, initialChoices, List (initialState))), None)
  }
}
