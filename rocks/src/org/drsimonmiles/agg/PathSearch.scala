package org.drsimonmiles.agg

import org.drsimonmiles.util.Measure.measure
import scala.annotation.tailrec
import scala.collection.immutable.{HashSet, TreeSet}

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
                                 (distance: State => Double)
                                 (getAvailableActions: State => Iterable[Action])
                                 (perform: (State, Action) => Option[State])
                                 (reachedGoal: State => Boolean): Option[Seq[Action]] = {
    // A list of actions performed plus the state reached
    type SolveState = (List[Action], State)

    // Cost function calculated as the length of the path traversed plus the distance from the solution
    def cost (state: SolveState) =
      state._1.length + distance (state._2)

    @tailrec
    def solve (states: TreeSet[SolveState], tried: Set[State]): Option[Seq[Action]] =
      if (states.isEmpty) None
      else {
        val state = states.head
        if (measure ("PS.reachedGoal", reachedGoal (state._2)))
          Some (state._1.reverse)
        else if (terminate ())
          None
        else {
          //println (s"St: ${state._2}")
          val actions = measure ("PS.getAvailableActions", getAvailableActions (state._2))
          //println ("Ac: " + actions)
          val performed = measure ("PS.performed", actions.flatMap (move => perform (state._2, move).map (result => new SolveState (move :: state._1, result))))
          //println ("Pe: " + performed.mkString ("\n"))
          val filtered = measure ("PS.filtered", performed.filter (state => !tried.contains (state._2)).toList)
          //println ("Fi: " + filtered.mkString ("\n"))
          val newPaths = measure ("PS.newPaths", states.tail ++ filtered) // (filtered ::: remaining).sortWith ((a, b) => cost (a) < cost (b)))
          //println ("NP: " + newPaths)
          val newTried = measure ("PS.newTried", tried ++ filtered.map (_._2))
          //println ("NT: " + newTried.mkString ("\n"))
          solve (newPaths, newTried)
        }
    }

    solve (TreeSet (new SolveState (Nil, start))((a, b) => (cost (a) - cost (b)).toInt), HashSet (start))
  }
}
