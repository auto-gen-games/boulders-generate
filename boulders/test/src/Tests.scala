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
          // Solve puzzle within time-out
          val game = Game (puzzle)
          val timeout = TimeOut.timeOutFromNow (10000L)
          val solution = Solve.solve (game)(timeout)(Definition.flipGameDefinition)
          println (Solve.toString (solution))
          // Assert puzzle solved
          assert (solution.isDefined)
        }
      }
    }
}
