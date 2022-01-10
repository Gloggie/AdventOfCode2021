import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day2 extends App {
  Using(Source.fromFile("inputs/input2")) {
    _.getLines().toList
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>
      //var horizontal = 0
      //var depth = 0

      val (horizontal, depth) = rows.foldLeft((0,0)) {
        case ((horizontal, depth), s"up $value") => (horizontal, depth - value.toInt)
        case ((horizontal, depth), s"down $value") => (horizontal, depth + value.toInt)
        case ((horizontal, depth), s"forward $value") => (horizontal + value.toInt, depth)
      }

      println(s"Part one = ${horizontal * depth}")

      val (horizontal2, depth2, _) = rows.foldLeft((0,0,0)) {
        case ((horizontal, depth, aim), s"up $value") => (horizontal, depth, aim - value.toInt)
        case ((horizontal, depth, aim), s"down $value") => (horizontal, depth, aim + value.toInt)
        case ((horizontal, depth, aim), s"forward $value") => (horizontal + value.toInt, depth + aim * value.toInt, aim)
      }

      println(s"Part two = ${horizontal2 * depth2}")

      /*horizontal = 0
      depth = 0
      var aim = 0

      rows foreach { command => {

        command match {
          case s"up $value" => aim -= value.toInt
          case s"down $value" => aim += value.toInt
          case s"forward $value" =>
            horizontal += value.toInt
            depth += aim * value.toInt
        }
      }
      }

      println(s"Part two = ${horizontal * depth}")*/
  }


}