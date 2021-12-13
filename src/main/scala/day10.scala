package day10

import scala.util.control.Breaks._

@main def day10: Unit = {
  val closingToOpeningMap = Map(
    ")" -> "(",
    "]" -> "[",
    "}" -> "{",
    ">" -> "<"
  )
  val openingToClosingMap = for ((k, v) <- closingToOpeningMap) yield (v, k)

  def findFirstIllegalCharacter(line: String): Option[String] = {
    var stack = List[String]()
    var firstIllegalCharacter: Option[String] = None

    breakable {
      for (item <- line.split("")) {
        stack = closingToOpeningMap.get(item) match {
          case Some(expectedOpening) => {
            if (expectedOpening != stack.head) {
              firstIllegalCharacter = Some(item)
              break
            }
            stack.tail
          }
          case None => item :: stack
        }
      }
    }

    firstIllegalCharacter
  }

  val lines = io.Source
    .fromResource("day10.txt")
    .getLines()
    .toSeq

  val syntaxCheckingScore = lines
    .map(findFirstIllegalCharacter)
    .map(maybeFirstIllegal => {
      maybeFirstIllegal match {
        case Some(item) =>
          item match {
            case ")" => 3
            case "]" => 57
            case "}" => 1197
            case ">" => 25137
          }
        case None => 0
      }
    })
    .sum

  println(s"Part 1 = ${syntaxCheckingScore}")

  def calculateAutocomplete(line: String): List[String] = {
    var stack = List[String]()

    for (item <- line.split("")) {
      stack = closingToOpeningMap.get(item) match {
        case Some(expectedOpening) => stack.tail
        case None                  => item :: stack
      }
    }

    // we now have what's left of the stack
    // it's in reverse order relative to the input so we traverse
    // it to find the closings without needing to reverse it first
    stack.map(item => openingToClosingMap(item))
  }

  val autocompleteValues = lines
    .filter(line => findFirstIllegalCharacter(line).isEmpty)
    .map(line => calculateAutocomplete(line))
    .map(autocomplete =>
      autocomplete.foldLeft(BigInt(0))((acc, item) => {
        val itemValue = item match {
          case ")" => 1
          case "]" => 2
          case "}" => 3
          case ">" => 4
        }
        acc * 5 + itemValue
      })
    )
    .sorted

  // copied from day7
  val median = if (autocompleteValues.length % 2 == 1) {
    // odd number of numbers - could floor but this is simpler
    val midpoint = (autocompleteValues.length - 1) / 2
    autocompleteValues(midpoint)
  } else { // shouldn't be necessary according to the instructions, but whatever
    val midpoint = autocompleteValues.length / 2
    (autocompleteValues(midpoint - 1) + autocompleteValues(midpoint)) / 2
  }

  println(s"Part 2 = ${median}")

}
