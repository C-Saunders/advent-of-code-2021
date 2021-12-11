package day09

import scala.collection.mutable

case class Position(row: Int, col: Int)

@main def day9: Unit = {
  val heightmap = mutable.Map[Position, Int]().withDefaultValue(Int.MaxValue)

  io.Source
    .fromResource("day9.txt")
    .getLines()
    .zipWithIndex
    .foreach((line, rowNum) => {
      line
        .split("")
        .map(_.toInt)
        .zipWithIndex
        .foreach((value, colNum) => {
          heightmap.update(Position(rowNum, colNum), value)
        })
    })

  def isLowestOfAdjacentPositions(position: Position, value: Int): Boolean = {
    val north = heightmap(Position(position.row - 1, position.col))
    val south = heightmap(Position(position.row + 1, position.col))
    val east = heightmap(Position(position.row, position.col + 1))
    val west = heightmap(Position(position.row, position.col - 1))

    Seq(north, south, east, west).forall(value < _)
  }

  val lowPoints = heightmap
    .filter((pos, value) => isLowestOfAdjacentPositions(pos, value))
    .toSeq

  val lowPointRiskLevels = lowPoints
    .map((_pos, value) => value + 1)
    .sum

  println(s"Part 1 = ${lowPointRiskLevels}")

  def getBasinPositions(
      position: Position,
      accumulator: mutable.Set[Position] = mutable.Set[Position]()
  ): mutable.Set[Position] = {
    val value = heightmap(position)
    if (value >= 9) { return accumulator }

    accumulator.add(position)

    val north = Position(position.row - 1, position.col)
    val south = Position(position.row + 1, position.col)
    val east = Position(position.row, position.col + 1)
    val west = Position(position.row, position.col - 1)

    accumulator.addAll(
      Seq(north, south, east, west)
        .filter(pos => heightmap(pos) > value) // don't do back down the slope
        .flatMap(getBasinPositions(_, accumulator))
    )
  }

  val basinSizes = lowPoints
    .map({ case (pos, _value) =>
      getBasinPositions(pos)
    })
    .map(basin => basin.size)
    .sorted(Ordering[Int].reverse)
    .take(3)
    .product

  println(s"Part 2 = ${basinSizes}")
}
