package day05

import java.lang.Math
import scala.collection.mutable.Map

case class Position(x: Int, y: Int)
case class Cell(count: Int = 0)

case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
  def isHorizontal: Boolean = {
    y1 == y2
  }

  def isVertical: Boolean = {
    x1 == x2
  }
}

object Line {
  val lineExtractRegex = raw"(\d+),(\d+) -> (\d+),(\d+)".r

  def fromString(str: String): Line = {
    str match {
      case lineExtractRegex(x1, y1, x2, y2) =>
        Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }
  }
}

@main def day5: Unit = {
  def calculate(predicate: (Line) => Boolean): Int = {
    val map = Map[Position, Cell]()

    val lines =
      io.Source
        .fromResource("day5.txt")
        .getLines()
        .map(Line.fromString(_))
        .filter(predicate)
        .foreach(line => {
          val (x1, y1, x2, y2) = Tuple.fromProductTyped(line)

          val xRange = Range.inclusive(x1, x2, if (x1 < x2) 1 else -1)
          val yRange = Range.inclusive(y1, y2, if (y1 < y2) 1 else -1)

          // maybe in reality we want an iterator, but this is fine, probably
          // since all lines are horizontal, vertical, or 45deg, we'll only have different length
          // iterators if max[XY] == min[XY], so we can fill in the zip with min or max and it doesn't matter
          xRange
            .zipAll(yRange, x1, y1)
            .foreach((xVal, yVal) => {
              val pos = Position(xVal, yVal)
              map
                .update(pos, Cell(map.getOrElseUpdate(pos, new Cell).count + 1))
            })
        })

    map.count((pos, cell) => cell.count >= 2)
  }

  println(s"Part 1 = ${calculate(l => l.isHorizontal || l.isVertical)}")
  println(s"Part 2 = ${calculate(_l => true)}")
}
