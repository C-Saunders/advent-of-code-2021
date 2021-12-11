package day01

@main def day1: Unit = {
  def countPairwiseIncreases(lines: Seq[Int]): Int = {
    lines
      .sliding(2)
      .count(pair => pair.head < pair.last)
  }

  def part2(lines: Seq[Int]): Int = {
    countPairwiseIncreases(
      lines
        .sliding(3)
        .map(_.sum)
        .toSeq
    )
  }

  val lines = io.Source.fromResource("day1.txt").getLines().toSeq.map(_.toInt)

  println(s"Part 1 = ${countPairwiseIncreases(lines)}")
  println(s"Part 2 = ${part2(lines)}")
}
