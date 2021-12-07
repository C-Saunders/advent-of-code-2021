package day3

import scala.collection.mutable
import scala.util.control.Breaks._
import scala.compiletime.ops.boolean

case class Counts(zeros: Int = 0, ones: Int = 0)

def getCountsByPosition(values: Seq[String]): mutable.Map[Int, Counts] = {
  val countsByPosition = mutable.Map[Int, Counts]()

  for (line <- values) {
    for ((value, position) <- line.split("").zipWithIndex) {
      val temp = countsByPosition.getOrElseUpdate(position, new Counts())
      if (value == "0") {
        countsByPosition(position) = Counts(temp.zeros + 1, temp.ones)
      } else {
        countsByPosition(position) = Counts(temp.zeros, temp.ones + 1)
      }
    }
  }
  countsByPosition
}

def part1(): Int = {
  val countsByPosition = getCountsByPosition(
    io.Source.fromResource("day3.txt").getLines().toSeq
  )

  def calculateRate(
      countsByPosition: mutable.Map[Int, Counts],
      predicate: (Int, Int) => String
  ): Int = {
    val str = countsByPosition
      .map((position, counts) => {
        val (zeros, ones) = Tuple.fromProductTyped(counts)
        predicate(zeros, ones)
      })
      .mkString

    Integer.parseInt(str, 2)
  }

  val gamma = calculateRate(
    countsByPosition,
    (zeros, ones) => {
      // most common
      if (zeros > ones) {
        "0"
      } else {
        "1"
      }
    }
  )
  val epsilon = calculateRate(
    countsByPosition,
    (zeros, ones) => {
      // least common
      if (zeros < ones) {
        "0"
      } else {
        "1"
      }
    }
  )

  gamma * epsilon
}

def getLifeSupportItemRating(
    predicate: (Char, Counts) => Boolean
): Int = {
  var position = 0
  var filtered = io.Source.fromResource("day3.txt").getLines().toSeq

  breakable {
    while (true) {
      val countsByPosition = getCountsByPosition(filtered)

      filtered = filtered
        .filter(number => {
          val counts = countsByPosition.get(position).get
          predicate(number.charAt(position), counts)
        })
        .toSeq

      if (filtered.length == 1) {
        break
      }
      position += 1
    }
  }

  Integer.parseInt(filtered.head, 2)
}

def part2(): Int = {
  val oxygenGeneratorRating =
    getLifeSupportItemRating((valueAtPosition, counts) => {
      // on a tie, say it's 1
      val mostCommon = (counts.ones >= counts.zeros) match {
        case true  => '1'
        case false => '0'
      }

      valueAtPosition == mostCommon
    })

  val co2scrubberRating =
    getLifeSupportItemRating((valueAtPosition, counts) => {
      // on a tie, say it's 0
      val leastCommon = (counts.zeros > counts.ones) match {
        case true  => '1'
        case false => '0'
      }
      valueAtPosition == leastCommon
    })

  oxygenGeneratorRating * co2scrubberRating
}

@main def day3: Unit = {
  println(s"Part 1 = ${part1()}")
  println(s"Part 2 = ${part2()}")
}
