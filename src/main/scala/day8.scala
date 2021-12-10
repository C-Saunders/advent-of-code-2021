package day8

def arrayMatch(a: Array[String], b: Array[String]): Boolean = {
  a.forall(b.contains) && b.forall(a.contains)
}

case class NumberPatterns(
    var zero: Array[String] = Array(),
    var one: Array[String] = Array(),
    var two: Array[String] = Array(),
    var three: Array[String] = Array(),
    var four: Array[String] = Array(),
    var five: Array[String] = Array(),
    var six: Array[String] = Array(),
    var seven: Array[String] = Array(),
    var eight: Array[String] = Array(),
    var nine: Array[String] = Array()
) {
  def getValue(array: Array[String]): Int = {
    if (array.length == 2) { return 1 }
    if (array.length == 3) { return 7 }
    if (array.length == 4) { return 4 }
    if (array.length == 7) { return 8 }

    if (array.length == 5) {
      if (arrayMatch(array, this.two)) { return 2 }
      if (arrayMatch(array, this.three)) { return 3 }
      if (arrayMatch(array, this.five)) { return 5 }
    }

    if (array.length == 6) {
      if (arrayMatch(array, this.zero)) { return 0 }
      if (arrayMatch(array, this.six)) { return 6 }
      if (arrayMatch(array, this.nine)) { return 9 }
    }

    throw new Exception(s"What happened? ${array.mkString}")
  }
}

@main def day8: Unit = {
  def toSingleCharacterArrays(item: String): Array[Array[String]] = {
    item.split(" ").map(_.trim).map(_.split(""))
  }

  def overlaps(haystack: Array[String], needle: Array[String]): Boolean = {
    needle.forall(haystack.contains)
  }

  /*
      Note: do not need to sort to do these operations
      all As are in B = A.forall(B.contains)
      A - B = A.diff(B) [does not include B - A]

      len 2 => one
      len 3 => seven
      len 4 => four
      len 5 => {
        overlaps one => three
        overlaps diff(four, one) => five
        else => two
      }
      len 6 => {
        does not overlap one => six
        overlaps four => nine
        else => zero
      }
   */
  def getPatternsFromSignals(signals: Array[Array[String]]): NumberPatterns = {
    val patterns = new NumberPatterns
    signals.foreach(signal => {
      signal.length match {
        case 2 => patterns.one = signal
        case 3 => patterns.seven = signal
        case 4 => patterns.four = signal
        case 5 => {
          if (overlaps(signal, patterns.one)) {
            patterns.three = signal
          } else if (overlaps(signal, patterns.four.diff(patterns.one))) {
            patterns.five = signal
          } else {
            patterns.two = signal
          }
        }
        case 6 => {
          if (!overlaps(signal, patterns.one)) {
            patterns.six = signal
          } else if (overlaps(signal, patterns.four)) {
            patterns.nine = signal
          } else {
            patterns.zero = signal
          }
        }
        case 7 => patterns.eight = signal
      }
    })

    patterns
  }

  val r = io.Source
    .fromResource("day8.txt")
    .getLines()
    .map(line => {
      val split = line.split(" \\| ")
      // sort by length so one and four are found before we need them to figure out the others
      val signals =
        toSingleCharacterArrays(split(0)).sortBy(item => item.length)
      val output = toSingleCharacterArrays(split(1))

      val patterns = getPatternsFromSignals(signals)

      output.map(patterns.getValue(_).toString()).mkString.toInt
    })
    .sum

  println(r)
}
