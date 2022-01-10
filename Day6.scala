import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day6 extends App {
  Using(Source.fromFile("inputs/input6")) {
    _.getLines().toList
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>
      val input = rows.head.split(",").map(_.toInt)

      def doCount(days: Int): Long = {
        var counts = collection.mutable.Map[Int, Long]()
        (0 to 8).foreach(c => counts += (c -> input.count(_ == c)))
        for (day <- 1 to days) {
          val newCounts = collection.mutable.Map[Int, Long]()
          counts foreach { case (x, c) => x match {
            case 0 =>
              newCounts += 8 -> c
              newCounts += 6 -> c
            case _ =>
              newCounts += x - 1 -> (newCounts.getOrElse(x - 1, 0L) + c)
          }
          }
          counts = newCounts
        }
        counts.foldLeft(0L)(_ + _._2)
      }

      println(s"Total size after 80 days: ${doCount(80)}")
      println(s"Total size after 256 days: ${doCount(256)}")

  }
}
