import scala.collection.mutable
import scala.util.control.Breaks._

case class Position(col: Int, row: Int)
case class Cell(value: Int, marked: Boolean = false)

class Board(fromRows: Seq[String]) {
  val map = mutable.Map[Position, Cell]()
  private var alreadyWon = false

  fromRows.zipWithIndex.foreach((row, rowNum) => {
    row
      .split(" ")
      .filter(item => !item.isBlank)
      .map(_.trim.toInt)
      .zipWithIndex
      .foreach((value, colNum) => {
        map.addOne(Position(colNum, rowNum), Cell(value))
      })
  })

  // returns whether marking resulted in a win
  def markValue(value: Int): Boolean = {
    map.find(_._2.value == value) match {
      case Some(position, cell) => {
        map.update(position, Cell(cell.value, true))
        if (alreadyWon) {
          return false
        }
        if (hasWon) {
          alreadyWon = true
          return true
        }
        false
      }
      case None => false
    }
  }

  def hasWon: Boolean = {
    (0 to 4).exists(row => {
      (0 to 4).forall(col => map.get(Position(col, row)).get.marked)
    }) ||
    (0 to 4).exists(col => {
      (0 to 4).forall(row => map.get(Position(col, row)).get.marked)
    })
  }

  def getSumOfUnmarkedCells: Int = {
    map.filter(!_._2.marked).map(_._2.value).sum
  }

  override def toString: String = {
    (0 to 4)
      .map(row => {
        (0 to 4)
          .map(col => map.get(Position(col, row)).get)
          .map(cell => {
            cell.marked match {
              case true  => s"${cell.value}*"
              case false => cell.value.toString
            }
          })
          .mkString(" ")
      })
      .mkString("\n")
  }
}

@main def day4: Unit = {
  val lines = io.Source.fromResource("day4.txt").getLines()
  val pulledNumbers = lines.next().split(",").map(_.trim.toInt)

  val boards = lines
    .filter(line => !line.isBlank)
    .grouped(5)
    .map(boardRows => {
      new Board(boardRows)
    })
    .toSeq

  breakable {
    for (pulledNumber <- pulledNumbers) {
      // doing a side-effect in the filter feels kinda icky, but not sure what would be better
      val winningBoardsFromPull =
        boards.filter(_.markValue(pulledNumber)).toSeq
      val allWinningBoards = boards.filter(_.hasWon).toSeq

      if (winningBoardsFromPull.length == 1 && allWinningBoards.length == 1) {
        println(s"Part 1\n")
        println(winningBoardsFromPull.head)
        println(s"Pulled number = ${pulledNumber}")
        println(
          s"Score = ${pulledNumber * winningBoardsFromPull.head.getSumOfUnmarkedCells}"
        )
      }

      if (allWinningBoards.length == boards.length) {
        println(s"\nPart 2")
        println(winningBoardsFromPull.head)
        println(s"Pulled number = ${pulledNumber}")
        println(
          s"Score = ${pulledNumber * winningBoardsFromPull.head.getSumOfUnmarkedCells}"
        )
        break
      }
    }
  }

}
