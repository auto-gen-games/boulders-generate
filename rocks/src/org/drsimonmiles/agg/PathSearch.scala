package org.drsimonmiles.agg

import org.drsimonmiles.util.Measure.measure
import scala.annotation.tailrec

object PathSearch {
  /**
    * Perform an a-star search.
    *
    * @param start The initial state.
    * @param terminate Returns true once a timeout has been reached.
    * @param distance Heuristic for the distance of the given state to the solution.
    * @param getAvailableActions The actions available in the given state.
    * @param perform Performs the action in the given state, returning the new state if valid.
    * @param reachedGoal Returns true if a goal state has been reached.
    * @tparam Action The type of possible actions.
    * @tparam State The type of the states in the search space.
    * @return The path to the solution state, if one is found within the timeout.
    */
  def aStarSearch[Action, State] (start: State)
                                 (terminate: () => Boolean)
                                 (pathLength: List[Action] => Double)
                                 (distance: State => Double)
                                 (getAvailableActions: State => Iterable[Action])
                                 (perform: (State, Action) => Option[State])
                                 (reachedGoal: State => Boolean): Option[Seq[Action]] = {
    // A list of actions performed plus the state reached
    type SolveState = (List[Action], State)

    // Cost function calculated as the length of the path traversed plus the distance from the solution
    def cost (state: SolveState) =
      pathLength (state._1) + distance (state._2)

    // Orders two states with lowest cost first
    def compare (stateA: SolveState, stateB: SolveState): Boolean =
      cost (stateA) < cost (stateB)

    // Returns true if the LHS reaches the same state as the RHS but in equal or less actions
    def subsumes (existing: (List[Action], State), newState: (List[Action], State)) =
      existing._2 == newState._2 && pathLength (existing._1) <= pathLength (newState._1)

    @tailrec
    def solve (states: Vector[SolveState], tried: Vector[SolveState]): Option[Seq[Action]] =
      if (states.isEmpty) None
      else {
        val state = states.head
        if (reachedGoal (state._2)) Some (state._1.reverse)
        else if (terminate ()) None
        else {
          val actions = getAvailableActions (state._2)
          val performed = actions.flatMap (move => perform (state._2, move).map (result => new SolveState (move :: state._1, result)))
          val filtered = performed.filter (state => !tried.exists (subsumes (_, state))).toList
          val newPaths = (states.tail ++ filtered).sortWith (compare)
          val newTried = tried ++ filtered
          solve (newPaths, newTried)
        }
    }

    solve (Vector (new SolveState (Nil, start)), Vector ((Nil, start)))
  }
}
