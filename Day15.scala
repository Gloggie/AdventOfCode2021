import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day15 extends App {
  case class Point(x: Int, y: Int)

  Using(Source.fromFile("inputs/input15")) {
    _.getLines().toArray.map(_.split("").map(_.toInt))
  } match {
    case Failure(f) => println(f)
    case Success(grid) =>
      val matrix = (for {
        x <- grid.indices
        y <- grid.indices
      } yield Point(x, y) -> grid(y)(x)).toMap

      def generatePoints(current: Point, risk: Int, length: Int): Seq[(Point, Int)] = {
        val cX = current.x
        val cY = current.y
        for {
          x <- 0 until 5
          y <- 0 until 5
        } yield (Point(cX + x * length, cY + y * length), if ((risk + x + y) > 9) risk + x + y - 9 else risk + x + y)
      }

      def generateBigMap(matrix: Map[Point, Int]): Map[Point, Int] = {
        val l = matrix.keys.maxBy(_.x).x + 1
        matrix.toSeq.foldLeft(mutable.Seq[(Point, Int)]())((s, p) => s.appendedAll(generatePoints(p._1, p._2, l))).toMap
      }

      def getNeighbours(cur: Point, graph: Map[Point, Int]): Seq[Point] = {
        Seq(Point(cur.x - 1, cur.y), Point(cur.x + 1, cur.y), Point(cur.x, cur.y - 1), Point(cur.x, cur.y + 1)).filter(graph.contains)
      }

      def shortestPath(start: Point, dest: Point, graph: Map[Point, Int]): Int = {
        val dis = mutable.Map.from(graph.map(p => p._1 -> Int.MaxValue))
        val vis = mutable.Map.from(graph.map(p => p._1 -> false))
        dis(start) = 0
        var priQ = mutable.PriorityQueue((start, 0))(Ordering.by(_._2)).reverse

        while (priQ.nonEmpty) {
          val (current, _) = priQ.dequeue()
          vis(current) = true
          for (n <- getNeighbours(current, graph).filter(p => !vis(p))) {
            val tDist = dis(current) + graph(n)
            if (n == dest) return tDist
            if (tDist < dis(n)) {
              priQ = priQ.filterNot(_._1 == n)
              priQ.enqueue((n, tDist))
              dis(n) = tDist
            }
          }
        }
        Int.MaxValue
      }

      var l = matrix.keys.maxBy(_.x).x
      val part1 = shortestPath(Point(0, 0), Point(l, l), matrix)
      println(s"Part1 = $part1")

      val bigMatrix = generateBigMap(matrix)
      l = bigMatrix.keys.maxBy(_.x).x
      val part2 = shortestPath(Point(0, 0), Point(l, l), bigMatrix)
      println(s"Part2 = $part2")
  }
}
