import scala.io.Source
import scala.util.{Failure, Success, Using}
import scala.util.control.Breaks._

object Day4 {
  Using(Source.fromFile("inputs/input4")) {
    _.getLines().toList
  } match {
    case Failure(f) => println(f)
    case Success(rows) =>

      val bingoNrs = rows.head.split(",").map(_.toInt)

      class Board(rows: Array[Array[Int]], cols: Array[Array[Int]]) {
        var myRows: Array[Array[Int]] = rows
        var myCols: Array[Array[Int]] = cols

        def mark(num: Int): Unit = {
          myRows = myRows.map(_.filter(_ != num))
          myCols = myCols.map(_.filter(_ != num))
        }

        def isBingo: Boolean = myRows.exists(_.isEmpty) || myCols.exists(_.isEmpty)

        def score: Int = myRows.flatten.sum

        def printBoard(): Unit = {
          println(s"Is bingo? $isBingo")
          myRows foreach {
            row => {
              row foreach {
                num => println(num)
              }
              println
            }
          }
        }
      }

      val boards = rows.tail.filter(_.nonEmpty).grouped(5).map(_.map(_.trim.split("\\s+").map(_.toInt).toArray).toArray).map(b => new Board(b, b.transpose)).toList

      breakable {
        bingoNrs foreach {
          num => {
            boards.foreach(_.mark(num))
            val gotBingo = boards.filter(_.isBingo)
            if (gotBingo.nonEmpty) {
              gotBingo.head.printBoard()
              println(s"BINGO!!! last $num score = ${gotBingo.map(_.score).max * num}")
              break
            }
          }
        }
      }

      var lastBingoScore: Int = 0
      bingoNrs foreach {
        num => {
          val leftBoards = boards.filter(!_.isBingo)
          leftBoards.foreach(_.mark(num))
          val gotBingo = leftBoards.filter(_.isBingo)
          if (gotBingo.nonEmpty) {
            println(s"BINGO!!! ${gotBingo.length} score = ${gotBingo.map(_.score).min * num}")
            lastBingoScore = gotBingo.map(_.score).max * num
          }
        }
      }

      println(s"BINGO!!! score = $lastBingoScore")
  }
}