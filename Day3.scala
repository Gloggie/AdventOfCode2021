import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day3 {
  Using(Source.fromFile("inputs/input3")) {
    _.getLines().toList
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>
      val ns: Array[Int] = new Array[Int](rows.head.length)

      for (x <- rows) {
        for (i <- 0 until x.length) {
          if (x.charAt(i) == '1') ns(i) += 1
        }
      }

      val getBit = (i: Int) => if (i > rows.length / 2) 1 else 0
      val getNegativeBit = (i: Int) => if (i > rows.length / 2) 0 else 1

      println(s"Part one = ${Integer.parseInt(ns.map(x => getBit(x)).mkString, 2) * Integer.parseInt(ns.map(x => getNegativeBit(x)).mkString, 2)}")

      var oxyList = rows
      var co2List = rows

      for (i <- 0 until rows.head.length) {
        if (oxyList.length > 1) {
          val oxyOnes = oxyList.count(x => x.charAt(i) == '1')
          val oxyZeros = oxyList.length - oxyOnes
          if (oxyOnes >= oxyZeros) {
            oxyList = oxyList.filter(x => x.charAt(i) == '1')
          } else {
            oxyList = oxyList.filter(x => x.charAt(i) == '0')
          }
        }

        if (co2List.length > 1) {
          val co2ones = co2List.count(x => x.charAt(i) == '1')
          val co2zeros = co2List.length - co2ones
          if (co2zeros <= co2ones) {
            co2List = co2List.filter(x => x.charAt(i) == '0')
          } else {
            co2List = co2List.filter(x => x.charAt(i) == '1')
          }
        }
      }

      val oxyRating = oxyList.head
      val co2Rating = co2List.head

      println(s"Part two = ${Integer.parseInt(oxyRating, 2) * Integer.parseInt(co2Rating, 2)}")
  }
}