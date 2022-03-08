import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day10 extends App {
  Using(Source.fromFile("inputs/input10")) {
    _.getLines().toList
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>
      val starts = Set("(","[","{","<")
      def isOpposite(start: String, end: String) = {
        (start.equals("(") && end.equals(")")) ||
          (start.equals("[") && end.equals("]")) ||
          (start.equals("{") && end.equals("}")) ||
          (start.equals("<") && end.equals(">"))
      }
      def getScore(end: String) = end match {
        case ")" => 3
        case "]" => 57
        case "}" => 1197
        case ">" => 25137
      }
      def getMissingScore(end: String) = end match {
        case "(" => 1
        case "[" => 2
        case "{" => 3
        case "<" => 4
      }
      def getCorruptScore(row: String): Int = {
        var buffer = Seq[String]()
        row.split("") foreach { c => {
          if (starts.contains(c))
            buffer = buffer.appended(c)
          else if (isOpposite(buffer.last, c))
            buffer = buffer.dropRight(1)
          else
            return getScore(c)
        }}
        0
      }
      def part1Score() = {
        rows.map(getCorruptScore).sum
      }
      def getIncompleteScore(row:String): Long = {
        var buffer = Seq[String]()
        row.split("") foreach { c => {
          if (starts.contains(c))
            buffer = buffer.appended(c)
          else if (isOpposite(buffer.last, c))
            buffer = buffer.dropRight(1)
          else
            return 0L
        }}
        buffer.reverse.map(getMissingScore).foldLeft(0L)((t,s) =>t*5+s)
      }

      def part2Score() = {
        val sortedScores = rows.map(getIncompleteScore).filter(_ > 0).sorted
        sortedScores(sortedScores.length/2)
      }

      println(s"Part 1 = ${part1Score()}")
      println(s"Part 2 = ${part2Score()}")
  }
}
