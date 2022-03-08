import java.math.BigInteger
import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day16 extends App {
  Using(Source.fromFile("inputs/input16")) {
    _.getLines().toSeq
  } match {
    case Failure(f) => println(f)
    case Success(input) =>
      val binArr = input.head.split("").foldLeft("")((s,h) => s + Integer.toBinaryString(Integer.parseInt(h,16)).reverse.padTo(4,'0').reverse).split("").map(_.toInt)

      def parsePacket(binArr: Seq[Int]): (Int,Long,Seq[Int]) = {
        val v = Integer.parseInt(binArr.slice(0,3).mkString(""),2)
        val t = Integer.parseInt(binArr.slice(3,6).mkString(""),2)
        t match {
          case 4 =>
            var sub = binArr.drop(6)
            val value = mutable.ArrayBuffer[Int]()
            while (sub.head == 1) {
              value ++= sub.slice(1,5)
              sub = sub.drop(5)
            }
            value ++= sub.slice(1,5)
            sub = sub.drop(5)
            (v,new BigInteger(value.mkString(""),2).longValue(),sub)
          case _ =>
            val i = binArr(6)
            val l = if (i == 0) Integer.parseInt(binArr.slice(7,22).mkString(""),2) else Integer.parseInt(binArr.slice(7,18).mkString(""),2)
            var sub = if (i == 0) binArr.slice(22,22+l) else binArr.slice(18,binArr.length)
            var total = v
            val values = mutable.ListBuffer[Long]()
            sub = i match {
              case 0 =>
                while (sub.nonEmpty) {
                  val (subV, value, newSub) = parsePacket(sub)
                  values += value
                  total += subV
                  sub = newSub
                }
                binArr.slice(22+l,binArr.length)
              case 1 =>
                for (_ <- 0 until l) {
                  val (subV, value, newSub) = parsePacket(sub)
                  values += value
                  total += subV
                  sub = newSub
                }
                sub
            }
            val res = t match {
              case 0 => values.sum
              case 1 => values.product
              case 2 => values.min
              case 3 => values.max
              case 5 => if (values.head > values(1)) 1 else 0
              case 6 => if (values.head < values(1)) 1 else 0
              case 7 => if (values.head == values(1)) 1 else 0
            }
            (total,res,sub)
        }
      }

      val (totalV,result,_) = parsePacket(binArr)
      println(s"Part1 = $totalV")
      println(s"Part2 = $result")
  }
}
