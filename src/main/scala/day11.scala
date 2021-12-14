package day11

import scala.collection.mutable.Map
import scala.util.control.Breaks._

case class Position(row: Int, col: Int)

@main def day11: Unit = {
  val map = Map[Position, Int]()

  io.Source
    .fromResource("day11.txt")
    .getLines()
    .zipWithIndex
    .foreach((line, row) => {
      line
        .split("")
        .zipWithIndex
        .foreach((value, col) => {
          map.addOne(Position(row, col), value.toInt)
        })
    })

  def executeStep(): Int = {
    // increase all values by 1
    map.mapValuesInPlace((_pos, value) => value + 1)

    var flashersForStep = Set[Position]()
    // find flashers
    // add to Set
    // propagate +1s from flashes
    // repeat until there are no flashers
    breakable {
      while (true) {
        val newFlashers =
          map
            .filter((pos, value) => value > 9 && !flashersForStep.contains(pos))
            .toMap

        if (newFlashers.size == 0) {
          break
        }

        flashersForStep = flashersForStep ++ newFlashers.keySet

        newFlashers.foreach((pos, _value) => {
          Range
            .inclusive(pos.row - 1, pos.row + 1)
            .foreach(rowNum => {
              Range
                .inclusive(pos.col - 1, pos.col + 1)
                .foreach(colNum => {
                  breakable {
                    if (rowNum == pos.row && colNum == pos.col) {
                      // break out of the closest breakable, meaning basically `continue`
                      break
                    }
                    val impactedPosition = Position(rowNum, colNum)
                    if (map.isDefinedAt(impactedPosition)) {
                      map.update(impactedPosition, map(impactedPosition) + 1)
                    }
                  }
                })
            })
        })
      }
    }

    // reset all flashers to 0
    map.mapValuesInPlace((pos, value) => {
      if (flashersForStep.contains(pos)) {
        0
      } else {
        value
      }
    })

    flashersForStep.size
  }

  var totalFlashes = 0
  var hundredSteps = false
  var allFlashed = false
  var stepCounter = 1

  while (!hundredSteps || !allFlashed) {
    val numberOfNewFlashes = executeStep()
    totalFlashes += numberOfNewFlashes
    if (stepCounter == 100) {
      println(s"Part 1 (total after 100 steps) = ${totalFlashes}")
      hundredSteps = true
    }

    if (numberOfNewFlashes == map.size) {
      println(s"Part 2 (first step with all flashes) = ${stepCounter}")
      allFlashed = true
    }

    stepCounter += 1
  }
}
