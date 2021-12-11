package day07

@main def day7: Unit = {
  val positions =
    io.Source
      .fromResource("day7.txt")
      .getLines()
      .next()
      .split(",")
      .map(_.toInt)
      .sorted
      .toVector

  val median = if (positions.length % 2 == 1) {
    // odd number of numbers - could floor but this is simpler
    val midpoint = (positions.length - 1) / 2
    positions(midpoint)
  } else {
    val midpoint = positions.length / 2
    (positions(midpoint - 1) + positions(midpoint)) / 2
  }

  def calculateFlatCost(positions: Vector[Int], alignmentPosition: Int): Int = {
    positions.map(pos => math.abs(pos - alignmentPosition)).sum
  }

  def calculateIncreasingCost(
      positions: Vector[Int],
      alignmentPosition: Int
  ): Int = {
    positions
      .map(pos => {
        val positionDifference = math.abs(pos - alignmentPosition)
        positionDifference * (positionDifference + 1) / 2
      })
      .sum
  }

  println(s"Part 1 = ${calculateFlatCost(positions, median)}")

  val minWithIncreasingCosts = Set
    .from(positions)
    .foldLeft(Int.MaxValue)((existingMin, newPosition) =>
      math.min(existingMin, calculateIncreasingCost(positions, newPosition))
    )

  println(s"Part 2 = ${minWithIncreasingCosts}")
}
