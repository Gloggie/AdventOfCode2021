import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day14 extends App {
  Using(Source.fromFile("inputs/input14")) {
    _.getLines().toList
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>
      val template = (rows.head + " ").sliding(2).toSeq.groupMapReduce(identity)(_ => 1L)(_+_)
      val rules = rows.tail.tail.map(s => {
        val ss = s.split(" -> ")
        ss(0) -> ss(1)
      }).toMap
      //for ((k,v) <- rules) println(s"key: $k, value: $v")

      def step(template: Map[String,Long]): Map[String,Long] = {
        template.toSeq.foldLeft(collection.mutable.Seq[(String,Long)]())((s, pair) => {
          rules.get(pair._1) match {
            case Some(c) => s.appended(s"${pair._1.head}$c" -> pair._2).appended(s"$c${pair._1.last}" -> pair._2)
            case None => s.appended(s"${pair._1.head} " -> 1L)
          }
        }
        ).groupMapReduce(_._1)(_._2)(_+_)
      }

      def takeNSteps(template: Map[String,Long], steps: Int): Map[String,Long] = {
        (0 until steps).foldLeft(template)((t,_) => step(t))
      }

      def calcResult(template: Map[String,Long]): Long = {
        val counts = template.groupMapReduce(_._1.head)(_._2)(_ + _).values
        counts.max - counts.min
      }

      val part1 = takeNSteps(template, 10)
      println(s"Part1 ${calcResult(part1)}")

      val part2 = takeNSteps(template, 40)
      println(s"Part2 ${calcResult(part2)}")

  }
}
