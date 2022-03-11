import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day16 extends App {
  Using(Source.fromFile("inputs/input16")) {
    _.getLines().toSeq.head.split("").foldLeft("")((s, h) => s + Integer.toBinaryString(Integer.parseInt(h, 16)).reverse.padTo(4, '0').reverse).split("").map(_.toInt).toSeq
  } match {
    case Failure(f) => println(f)
    case Success(input) =>
      def mapOperator(oppType: Int, values: Seq[Long]): Long = {
        oppType match {
          case 0 => values.sum
          case 1 => values.product
          case 2 => values.min
          case 3 => values.max
          case 5 => if (values.head > values(1)) 1 else 0
          case 6 => if (values.head < values(1)) 1 else 0
          case 7 => if (values.head == values(1)) 1 else 0
        }
      }

      def parseSubPackets(v: Int, t: Int, packet: Seq[Int]): (Int, Long, Seq[Int]) = {
        val i = packet(6)
        val l = if (i == 0) Integer.parseInt(packet.slice(7, 22).mkString(""), 2) else Integer.parseInt(packet.slice(7, 18).mkString(""), 2)
        var sub = if (i == 0) packet.slice(22, 22 + l) else packet.slice(18, packet.length)
        var total = v
        val values = mutable.ListBuffer[Long]()
        if (i == 0) {
          while (sub.nonEmpty) {
            val (subV, value, newSub) = parsePacket(sub)
            values += value
            total += subV
            sub = newSub
          }
          sub = packet.slice(22 + l, packet.length)
        } else {
          for (_ <- 0 until l) {
            val (subV, value, newSub) = parsePacket(sub)
            values += value
            total += subV
            sub = newSub
          }
        }
        val res = mapOperator(t, values.toSeq)
        (total, res, sub)
      }

      def parsePacket(packet: Seq[Int]): (Int, Long, Seq[Int]) = {
        val v = Integer.parseInt(packet.slice(0, 3).mkString(""), 2)
        val t = Integer.parseInt(packet.slice(3, 6).mkString(""), 2)
        t match {
          case 4 =>
            var sub = packet.drop(6)
            val value = mutable.ArrayBuffer[Int]()
            while (sub.head == 1) {
              value ++= sub.slice(1, 5)
              sub = sub.drop(5)
            }
            value ++= sub.slice(1, 5)
            sub = sub.drop(5)
            (v, java.lang.Long.parseLong(value.mkString(""), 2), sub)
          case _ =>
            parseSubPackets(v, t, packet)
        }
      }

      val (totalV, result, _) = parsePacket(input)
      println(s"Part1 = $totalV")
      println(s"Part2 = $result")
  }
}
