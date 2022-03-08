import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day11 extends App {
  Using(Source.fromFile("inputs/input11")) {
    _.getLines().toArray.map(_.split("").map(_.toInt))
  } match {
    case Failure(f) => println(f)
    case Success(matrix) =>
      matrix.foreach(s => println(s.mkString("")))
      println()

      def takeStep(state: Array[Array[Int]]) = {
        val newState = state.map(_.map(_ + 1))
        while (newState.flatten.max > 9) {
          for (y <- newState.indices;
               x <- newState(0).indices) {
            if (newState(y)(x) > 9) {
              newState(y)(x) = 0
              for (a <- Math.max(y-1,0) to Math.min(y+1,newState.length-1);
                   b <- Math.max(x-1,0) to Math.min(x+1,newState(y).length-1)
                   ) {
                newState(a)(b) = if (newState(a)(b) == 0) 0 else newState(a)(b) + 1
              }
            }
          }
        }
        newState
      }

      def takeSteps(n:Int) = {
        var state = matrix
        var flashed = 0
        for (_ <- 0 until n) {
          state = takeStep(state)
          flashed += state.flatten.count(_ == 0)
        }
        flashed
      }

      def findSyncedFlash() = {
        var step = 0
        var state = matrix
        while (state.flatten.max > 0) {
          step += 1
          state = takeStep(state)
        }
        step
      }

      println(s"Part1 = ${takeSteps(100)}")
      println(s"Part2 = ${findSyncedFlash()}")
  }
}
