package day2

object Direction extends Enumeration {
  type Direction = Value
  val Forward, Up, Down, Unknown = Value

  def withNameWithDefault(name: String): Value =
    values
      .find(_.toString.toLowerCase() == name.toLowerCase())
      .getOrElse(Unknown)
}

case class Instruction(direction: Direction.Value, amount: Int)

class Submarine(var distance: Int = 0, var depth: Int = 0, var aim: Int = 0)

@main def day2: Unit = {
  val instructions = io.Source
    .fromResource("day2.txt")
    .getLines()
    .map(line => {
      val split = line.split(" ")
      Instruction(Direction.withNameWithDefault(split.head), split.last.toInt)
    })
    .toSeq

  def part1(instructions: Seq[Instruction]): Int = {
    val submarine = new Submarine

    for (instruction <- instructions) {
      instruction.direction match {
        case Direction.Forward => submarine.distance += instruction.amount
        case Direction.Up      => submarine.depth -= instruction.amount
        case Direction.Down    => submarine.depth += instruction.amount
      }
    }

    submarine.depth * submarine.distance
  }

  def part2(instructions: Seq[Instruction]): Int = {
    val submarine = new Submarine

    for (instruction <- instructions) {
      instruction.direction match {
        case Direction.Forward => {
          submarine.distance += instruction.amount
          submarine.depth += submarine.aim * instruction.amount
        }
        case Direction.Up   => submarine.aim -= instruction.amount
        case Direction.Down => submarine.aim += instruction.amount
      }
    }

    submarine.depth * submarine.distance
  }

  println(s"Part 1 = ${part1(instructions)}")
  println(s"Part 2 = ${part2(instructions)}")
}
