package day06

case class Population(
    var zeros: BigInt = 0,
    var ones: BigInt = 0,
    var twos: BigInt = 0,
    var threes: BigInt = 0,
    var fours: BigInt = 0,
    var fives: BigInt = 0,
    var sixes: BigInt = 0,
    var sevens: BigInt = 0,
    var eights: BigInt = 0
) {
  def totalPopulation: BigInt = {
    zeros + ones + twos + threes + fours + fives + sixes + sevens + eights
  }

  def ageByOneDay: Unit = {
    val oldZeros = zeros
    val oldOnes = ones
    val oldTwos = twos
    val oldThrees = threes
    val oldFours = fours
    val oldFives = fives
    val oldSixes = sixes
    val oldSevens = sevens
    val oldEights = eights

    zeros = oldOnes
    ones = oldTwos
    twos = oldThrees
    threes = oldFours
    fours = oldFives
    fives = oldSixes
    sixes = oldSevens + oldZeros
    sevens = oldEights
    eights = oldZeros
  }
}

@main def day6: Unit = {
  val population = io.Source
    .fromResource("day6.txt")
    .getLines()
    .next()
    .split(",")
    .map(_.toInt)
    .foldLeft(new Population)((currPop, nextVal) => {
      nextVal match {
        case 0 => currPop.zeros += 1
        case 1 => currPop.ones += 1
        case 2 => currPop.twos += 1
        case 3 => currPop.threes += 1
        case 4 => currPop.fours += 1
        case 5 => currPop.fives += 1
        case 6 => currPop.sixes += 1
      }
      currPop
    })

  println(s"Initial population: ${population.totalPopulation} - ${population}")
  (0 to 79).foreach(_x => population.ageByOneDay)
  println(s"After 80 days: ${population.totalPopulation} - ${population}")
  (80 to 255).foreach(_x => population.ageByOneDay)
  println(s"After 256 days: ${population.totalPopulation} - ${population}")
}
