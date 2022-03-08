import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day9 extends App {
  Using(Source.fromFile("inputs/input9")) {
    _.getLines().toList
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>
      val heightMap = rows.map(_.split("").map(_.toInt).toArray).toArray

      def getNeighbours(x:Int, y:Int): Seq[Int] = {
        val neighbours = scala.collection.mutable.ListBuffer[Int]()
        if (x > 0) neighbours += heightMap(y)(x-1)
        if (x < heightMap(y).length-1) neighbours += heightMap(y)(x+1)
        if (y > 0) neighbours += heightMap(y-1)(x)
        if (y < heightMap.length-1) neighbours += heightMap(y+1)(x)

        neighbours.toSeq
      }

      def isLowPoint(x:Int, y:Int): Boolean = heightMap(y)(x) < getNeighbours(x,y).min

      val lowPoints = (for {
        x <- heightMap(0).indices
        y <- heightMap.indices
      } yield (x, y)).filter(t => isLowPoint(t._1, t._2))

      def part1: Long = lowPoints.map(t => heightMap(t._2)(t._1).toLong +1L).sum

      def getBasinNeighbours(x:Int,y:Int, exclude:Set[(Int,Int)]) = {
        val neighbours = scala.collection.mutable.Set[(Int,Int)]()
        if (x > 0 && heightMap(y)(x-1) < 9 && !exclude.contains((x-1,y))) neighbours += Tuple2(x-1,y)
        if (x < heightMap(y).length-1 && heightMap(y)(x+1) < 9 && !exclude.contains((x+1,y))) neighbours += Tuple2(x+1,y)
        if (y > 0 && heightMap(y-1)(x) < 9 && !exclude.contains((x,y-1))) neighbours += Tuple2(x,y-1)
        if (y < heightMap.length-1 && heightMap(y+1)(x) < 9 && !exclude.contains((x,y+1))) neighbours += Tuple2(x,y+1)

        neighbours
      }

      def getBasinSize(coordinate: (Int,Int)): Long = {
        val basin = scala.collection.mutable.Set[(Int,Int)]((coordinate._1,coordinate._2))
        var newCoordinates = scala.collection.mutable.Set[(Int,Int)]()
        do {
          newCoordinates = scala.collection.mutable.Set[(Int,Int)]()
          basin.foreach(coordinate => newCoordinates.addAll(getBasinNeighbours(coordinate._1,coordinate._2,basin.toSet)))
          basin.addAll(newCoordinates)
        } while (newCoordinates.nonEmpty)

        basin.size
      }

      def part2: Long = lowPoints.map(getBasinSize).sorted(Ordering.Long.reverse).take(3).product

      println(s"Part 1 = $part1")
      println(s"Part 2 = $part2")
  }
}