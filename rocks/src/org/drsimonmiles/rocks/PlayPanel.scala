package org.drsimonmiles.rocks

import java.awt.{Color, Dimension, Image}
import java.io.File
import javax.imageio.ImageIO
import org.drsimonmiles.rocks.Game._
import org.drsimonmiles.rocks.Play.{game, nextPuzzle, resetPuzzle}
import scala.swing._
import scala.swing.event.{Key, KeyPressed}

class PlayPanel extends Panel {
  val cellEdge = 40
  val padding = 10
  val boulder = ImageIO.read (new File ("img/boulder.png")).getScaledInstance (cellEdge, cellEdge, Image.SCALE_FAST)

  preferredSize = new Dimension (500, 500)
  opaque = true
  background = Color.WHITE
  listenTo (keys)

  reactions += {
    case KeyPressed (_, Key.A, _, _) if game.isDefined => move (Left)
    case KeyPressed (_, Key.D, _, _) if game.isDefined => move (Right)
    case KeyPressed (_, Key.Q, _, _) if game.isDefined => move (JumpLeft)
    case KeyPressed (_, Key.E, _, _) if game.isDefined => move (JumpRight)
    case KeyPressed (_, Key.W, _, _) if game.isDefined => move (Jump)
    case KeyPressed (_, Key.Key0, _, _) if game.isDefined => resetPuzzle (); repaint ()
  }

  focusable = true
  requestFocus ()

  def move (move: Move): Unit = {
    perform (game.get, move).foreach { g => game = Some (fallToGround (g)) }
    if (game.get.hasWon)
      nextPuzzle ()
    repaint ()
  }

  override def paintComponent (graphics: Graphics2D): Unit = {
    def near (cell: Int) = padding + cell * cellEdge
    def far (cell: Int) = near (cell + 1) - 1
    def cellLine (x: Int, y: Int, horizontal: Boolean, vertical: Boolean, topLeft: Boolean, shortening: Int = 0): Unit = {
      val x1 = if (!horizontal && !topLeft) far (x) - shortening else near (x) + shortening
      val y1 = if (horizontal && !topLeft) far (y) - shortening else near (y) + shortening
      val x2 = if (!horizontal) x1 else far (x) - shortening
      val y2 = if (!vertical) y1 else if (horizontal && !topLeft) near (y) + shortening else far (y) - shortening
      graphics.drawLine (x1, y1, x2, y2)
    }
    game match {
      case Some(g) =>
        import g._
        for (x <- 0 until width; y <- 0 until height) {
          if (hasBoulder(x, y)) graphics.drawImage (boulder, near (x), near (y), Color.WHITE, null) //graphics.drawOval (near (x), near (y), cellEdge - 1, cellEdge - 1)
          if (hasFloor(x, y)) cellLine (x, y, true, false, false)
          if (hasLeftWall(x, y)) cellLine (x, y, false, true, true)
          if (y == 0) cellLine (x, y, true, false, true)
          if (x == width - 1) cellLine (x, y, false, true, false)
          if (hasMan(x, y)) graphics.fillOval (near (x), near (y), cellEdge - 1, cellEdge - 1)
          if (hasExit(x, y)) {
            cellLine (x, y, true, true, true, 2)
            cellLine (x, y, true, true, false, 2)
          }
          if (hasStar (x, y)) {
            cellLine (x, y, true, true, true, 2)
            cellLine (x, y, true, true, false, 2)
            graphics.drawLine ((far (x) + near (x)) / 2, near (y), (far (x) + near (x)) / 2, far (y))
            graphics.drawLine (near (x), (far (y) + near (y)) / 2, far (x), (far (y) + near (y)) / 2)
          }
        }
      case None =>
    }
  }
}
