import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day8 extends App {
  Using(Source.fromFile("inputs/input8")) {
    _.getLines().toList
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>
      val data = rows.map(_.split(" \\| ").map(_.split(" ")))
      val v1478 = Set(2, 4, 3, 7)
      val part1count = data.map(_.count(x => v1478.contains(x.length))).sum

      println(s"Part1 = $part1count")

      def decrypt(line: Array[Array[String]]): Long = {
        val signal = line(0)
        val output = line(1)
        val patterns = scala.collection.mutable.Map[Int, String]()
        patterns += 1 -> signal.find(_.length == 2).get
        patterns += 4 -> signal.find(_.length == 4).get
        patterns += 7 -> signal.find(_.length == 3).get
        patterns += 8 -> signal.find(_.length == 7).get
        patterns += 6 -> signal.find(x => x.length == 6 && (!x.contains(patterns(1).charAt(0)) || !x.contains(patterns(1).charAt(1)))).get
        patterns += 3 -> signal.find(x => x.length == 5 && x.contains(patterns(1).charAt(0)) && x.contains(patterns(1).charAt(1))).get
        patterns += 9 -> signal.find(x => x.length == 6 && x.chars().toArray.count(c => !patterns(3).contains(c)) == 1).get
        patterns += 0 -> signal.find(x => x.length == 6 && !x.equals(patterns(6)) && !x.equals(patterns(9))).get
        patterns += 5 -> signal.find(x => x.length == 5 && patterns(6).count(c => !x.contains(c)) == 1).get
        patterns += 2 -> signal.find(x => x.length == 5 && !x.equals(patterns(5)) && !x.equals(patterns(3))).get

        val patterns2 = patterns.map(x => (x._2.sorted,x._1))

        val v = s"${patterns2.find(x => x._1.equals(output(0).sorted)).get._2}${patterns2.find(x => x._1.equals(output(1).sorted)).get._2}${patterns2.find(x => x._1.equals(output(2).sorted)).get._2}${patterns2.find(x => x._1.equals(output(3).sorted)).get._2}"
        //println(v)
        v.toLong
      }

      val part2count = data.map(decrypt).sum
      println(s"Part2 = $part2count")
  }
}
