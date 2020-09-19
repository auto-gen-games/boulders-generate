import org.drsimonmiles.rocks.{Definition, Game, Puzzle, Solve}
import org.drsimonmiles.util.TimeOut
import utest._

object Tests extends TestSuite {
  val tests: Tests =
    utest.Tests {
      "flip solving" - {
        "solve simple flip puzzle" - {
          // Create puzzle only possible with flip
          val code = "3,3;1,2;2,0;0,0;CAACAAFDD"
          val puzzle = Puzzle.fromCode (code).get
          val game = Game (puzzle)
          val solution = Solve.solve (game)(Definition.flipGameDefinition)
          // Assert puzzle solved
          assert (solution.isDefined)
        }
        "insoluble puzzle" - {
          val code = "5,5;2,0;1,3;4,4;CEDCACEAAAFDDAACDDDDFEDDD"
          val puzzle = Puzzle.fromCode (code).get
          val game = Game (puzzle)
          val solution = Solve.solve (game)(Definition.flipGameDefinition)
          assert (solution.isEmpty)
        }
      }
    }
}
