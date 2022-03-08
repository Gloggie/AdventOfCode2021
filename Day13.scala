import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day13 extends App {
  Using(Source.fromFile("inputs/input13")) {
    _.getLines().toList
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>
      case class Coordinate(x:Int, y:Int)

      val coordinates = rows.takeWhile(_.nonEmpty).map(_.split(",").map(_.toInt)).map(c => Coordinate(c(0),c(1)))
      val folds = rows.splitAt(rows.indexOf("")+1)._2.map(_.replace("fold along ","").split("="))

      def doFold(coordinates: List[Coordinate], foldAt: Array[String]): List[Coordinate] = {
        val f = foldAt(1).toInt
        (foldAt(0) match {
          case "y" => coordinates.map(c => if (c.y<f) c else Coordinate(c.x, f - Math.abs(f-c.y)))
          case "x" => coordinates.map(c => if (c.x<f) c else Coordinate(f - Math.abs(f-c.x), c.y))
        }).distinct
      }

      def doAllFolds(coordinates: List[Coordinate], folds: List[Array[String]]): List[Coordinate] = {
        folds.foldLeft(coordinates)((c,f) => doFold(c,f))
      }

      val part1 = doFold(coordinates, folds.head)
      println(s"Part1 = ${part1.size}")

      def maxX(coordinates: List[Coordinate]) = coordinates.map(_.x).max
      def maxY(coordinates: List[Coordinate]) = coordinates.map(_.y).max

      val part2 = doAllFolds(coordinates, folds)
      println(s"Part2 = ${part2.size}")
      for (y <- 0 to maxY(part2)) {
        val s = (0 to maxX(part2)).foldLeft("")((s,x) => {
          s + (if (part2.count(c => c.y == y && c.x == x) == 1) "# " else "  ")
        })
        println(s)
      }

  }
}
