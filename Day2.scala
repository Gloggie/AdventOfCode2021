import scala.io.Source

object Day2 extends App {
  val rows = Source.fromFile("input2").getLines.toList

  var horizontal = 0
  var depth = 0

  rows foreach { command => {
    val com = command.split(" ")
    println(s"${com(0)} ${com(1)}")
    com(0) match {
        case "up" => depth -= com(1).toInt
        case "down" => depth += com(1).toInt
        case "forward" => horizontal += com(1).toInt
    }
    println(s"horizontal = ${horizontal}")
    println(s"depth = ${depth}")
  }}
  println(s"horizontal = ${horizontal}")
  println(s"depth = ${depth}")

  println(s"Part one = ${horizontal * depth}")

  horizontal = 0
  depth = 0
  var aim = 0

  rows foreach { command => {
    val com = command.split(" ")
    println(s"${com(0)} ${com(1)}")
    val x = com(1).toInt
    com(0) match {
        case "up" => aim -= x
        case "down" => aim += x
        case "forward" => {
            horizontal += x
            depth += aim * x
        }
    }
  }}

  println(s"Part two = ${horizontal * depth}")
}