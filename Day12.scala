import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day12 extends App {
  Using(Source.fromFile("inputs/input12")) {
    _.getLines().toList.map(_.split("-"))
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>
      var caveMap = Map[String,List[String]]()
      rows.foreach(row => {
        if (!caveMap.contains(row(0)))
          caveMap = caveMap + (row(0) -> List(row(1)))
        else
          caveMap = caveMap + (row(0) -> caveMap(row(0)).appended(row(1)))
        if (!caveMap.contains(row(1)))
          caveMap = caveMap + (row(1) -> List(row(0)))
        else
          caveMap = caveMap + (row(1) -> caveMap(row(1)).appended(row(0)))
      })
      caveMap.foreachEntry((k,v) => {
        println(s"$k -> ${v.mkString(",")}")
      })

      def isBig(cave: String) = cave.toUpperCase.equals(cave)
      def isSmall(cave: String) = !isBig(cave)
      def hasSmallDuplicate(path: List[String]) = path.filter(isSmall).toSet.size < path.count(isSmall)

      def takeSteps1(paths: List[List[String]]) = {
        var newPaths = List[List[String]]()
        paths.foreach(path => {
          if (path.last.equals("end"))
            newPaths = newPaths.appended(path)
          else
            caveMap(path.last).filter(p => isBig(p) || !path.contains(p)).foreach(p => newPaths = newPaths.appended(path.appended(p)))
        })
        newPaths
      }

      def takeSteps2Rec(path: List[String]):List[List[String]] = {
        if (path.last.equals("end")) {
          List(path)
        } else {
            caveMap(path.last).filterNot(_.equals("start")).filter(p => isBig(p) || !hasSmallDuplicate(path) || !path.contains(p)).flatMap(p => takeSteps2Rec(path :+ p))
        }
      }

      def findPaths() = {
        var paths = List[List[String]]()
        caveMap("start").foreach(p => paths = paths.appended(List("start",p)))
        while (paths.count(_.last.equals("end")) < paths.size) {
          paths = takeSteps1(paths)
        }
        paths
      }

      //val paths = findPaths(takeSteps1)
      println()
      println(s"Part1 = ${findPaths().size}")
      println(s"Part2 = ${takeSteps2Rec(List("start")).size}")

  }
}
