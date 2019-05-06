package org.drsimonmiles.itapprox

/** Represents a decision to make in a system with a state of type State. */
trait Decision[State] {
  /** Enacts the decision in a given state, returning a new state if successful. */
  def enact (state: State): Option[State]
  /** Returns the opposite of this decision. */
  def not: Decision[State]
}

/** The decision to take both of the given decisions */
final case class Both[State] (decisionA: Decision[State], decisionB: Decision[State]) extends Decision[State] {
  def enact (state: State): Option[State] = decisionA.enact (state).flatMap (decisionB.enact)
  def not: Decision[State] = Both (decisionA.not, decisionB.not)
}

object Decision {
  /** Given a decision, return a choice where this decision is the only option. */
  def only[State] (decision: Decision[State]): Choice[State] =
    Choice (decision)

  /** Given a decision, return the choice between that decision and its opposite. */
  def decideOn[State] (decision: Decision[State]): Choice[State] =
    Choice (decision, decision.not)

  /** Choice between the given decision and its opposite, but preferring (trying first) the given decision). */
  def preferredDecision[State] (decision: Decision[State]): Choice[State] =
    Choice.preferential (decision, decision.not)

  /** Given two decisions, return the decision to do both. */
  def and[State] (decisionA: Decision[State], decisionB: Decision[State]): Decision[State] =
    Both (decisionA, decisionB)

  /** Given two decisions, returns the choice between performing both, or performing either one with the opposite of the other. */
  def or[State] (decisionA: Decision[State], decisionB: Decision[State]): Choice[State] =
    Choice (and (decisionA, decisionB.not), and (decisionA, decisionB), and (decisionA.not, decisionB))

  /** Given two decisions, returns the choice between performing both, or performing either one with the opposite of the other,
    * but biased towards just the first decision and not the second by the given likelihood. */
  def firstBiasedOr[State] (bias: Double, decisionA: Decision[State], decisionB: Decision[State]): Choice[State] =
    Choice.biased (bias, and (decisionA, decisionB.not), and (decisionA, decisionB), and (decisionA.not, decisionB))

  /** Given two decisions, returns the choice between performing both, or performing either one with the opposite of the other. */
  def xor[State] (decisionA: Decision[State], decisionB: Decision[State]): Choice[State] =
    Choice (and (decisionA.not, decisionB), and (decisionA, decisionB.not))
}
