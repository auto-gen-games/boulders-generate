package org.drsimonmiles.rocks

import org.drsimonmiles.rocks.PuzzleCreation.createPuzzle
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.swing.event.ButtonClicked

object Play extends SimpleSwingApplication {
  var puzzles: List[Puzzle] = Nil
  var current: Int = -1
  var game: Option[Game] = None
  val indexLabel = new Label ("Puzzle to be loaded")

  override def main (arguments: Array[String]): Unit = {
    puzzles = IO.load (Configuration.puzzlesFile)
    println (puzzles.size + " puzzles")
    showPuzzle (0)
    Future { while (true) createPuzzle () }
    super.main (arguments)
  }

  def showPuzzle (number: Int): Unit =
    if (puzzles.length > number && number >= 0) {
      game = Some (Game (puzzles (number)))
      current = number
      indexLabel.text = "Puzzle " + current
    }

  def nextPuzzle (): Unit =
    showPuzzle ((current + 1) % puzzles.length)

  def previousPuzzle (): Unit =
    if (current == 0) showPuzzle (puzzles.length - 1) else showPuzzle (current - 1)

  def resetPuzzle (): Unit =
    showPuzzle (current)

  def top = new MainFrame {
    title = "Boulders"
    val playPanel = new PlayPanel
    val nextButton = new Button ("Next")
    val prevButton = new Button ("Previous")
    val resetButton = new Button ("Restart")
    val buttons = new FlowPanel {
      contents += nextButton
      contents += prevButton
      contents += resetButton
    }
    contents = new BorderPanel {
      layout (indexLabel) = North
      layout (playPanel) = Center
      layout (buttons) = South
    }
    listenTo (nextButton)
    listenTo (prevButton)
    listenTo (resetButton)

    def repaintAction (action: () => Unit): Unit = {
      action ()
      repaint ()
      playPanel.requestFocus ()
    }
    reactions += {
      case ButtonClicked (component) if component == nextButton => repaintAction (nextPuzzle)
      case ButtonClicked (component) if component == prevButton => repaintAction (previousPuzzle)
      case ButtonClicked (component) if component == resetButton => repaintAction (resetPuzzle)
    }
    centerOnScreen ()
  }
}
