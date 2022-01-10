import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day5 extends App {
  Using(Source.fromFile("inputs/input5")) {
    _.getLines().toList
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>
      val points: mutable.Map[(Int, Int), Int] = mutable.Map()

      rows.map { case s"$x1,$y1 -> $x2,$y2" => (x1.toInt, x2.toInt, y1.toInt, y2.toInt) }.foreach {
        case (x1: Int, x2: Int, y1: Int, y2: Int) =>
          //println(r)
          if (x1 == x2) {
            for (y <- y1 to y2 by (y2 - y1).sign) {
              points((x1, y)) = if (!points.contains((x1, y))) 1 else points((x1, y)) + 1
            }
          } else if (y1 == y2) {
            for (x <- x1 to x2 by (x2 - x1).sign) {
              points((x, y1)) = if (!points.contains((x, y1))) 1 else points((x, y1)) + 1
            }
          } else {
            var x = x1
            var y = y1
            for (i <- 0 to (Math.max(x1, x2) - Math.min(x1, x2))) {
              x = if (x1 > x2) x1 - i else x1 + i
              y = if (y1 > y2) y1 - i else y1 + i
              points((x, y)) = if (!points.contains((x, y))) 1 else points((x, y)) + 1
            }
          }
      }

      println(s"Part two ${points.count(m => m._2 >= 2)}")
  }
}