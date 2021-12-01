import scala.io.Source

object Day1 extends App {
  val rows = Source.fromFile("input1").getLines.toList
  val measurements = rows.map((x:String) => x.toInt)

  // Part One
  var prevMeas = 9999999
  var largerCount = 0
  measurements foreach {
      m => {
      if (m > prevMeas) largerCount += 1
      prevMeas = m
      }
  }

  println(s"Part one = ${largerCount}")

  // Part Two
  prevMeas = 9999999
  largerCount = 0
  for (i <- 2 until measurements.length) {
    val m = measurements(i-2) + measurements(i-1) + measurements(i)
    if (m > prevMeas) largerCount += 1
    prevMeas = m
  }

  println(s"Part two = ${largerCount}")
}
