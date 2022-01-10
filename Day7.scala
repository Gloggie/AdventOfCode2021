import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day7 extends App {
  Using(Source.fromFile("inputs/input7")) {
    _.getLines().toList
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>
      val input = rows.head.split(",").map(_.toInt)
      def cost : Int = {
        (input.min to input.max).map(pos => input.foldLeft(0){(sum,x) => sum+(x-pos).abs}).min
      }
      def cost2 : Int = {
        (input.min to input.max).map(pos => input.foldLeft(0){(sum,x) => {
          val diff = (x-pos).abs
          sum + (diff * (diff + 1) / 2)
        }}).min
      }

      println(cost)
      println(cost2)
  }
}
