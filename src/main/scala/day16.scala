package day16

object InputSplitter {
  def split(str: Iterator[String]) : (List[String], List[String], List[String]) = {
    val list = str.toList
    val (rules, rest) = list.span(line => line != "")

    val (p2, rest2) = rest.drop(1).span(line => line != "")
    val mine = p2.drop(1)
    val p3 = rest2.drop(1)

    val tickets = p3.drop(1)

    (rules, mine, tickets)
  }
}

class RuleSet(val ranges: Map[String, List[Range]]) {

  def isValid(ticket: String) : Boolean = {
    ticket
      .split(",")
      .map(_.toInt)
      .forall(isFieldValid(_))
  }

  def isFieldValid(field: Int) : Boolean = {
    ranges.values.exists(ruleList => ruleList.exists(range => range.contains(field)))
  }

  def invalidFields(ticket: String) : List[Int] = {
    ticket
      .split(",")
      .map(_.toInt)
      .filter(field => !isFieldValid(field))
      .toList
  }

  def validFields(value: Int) : Set[String] = {
    ranges
      .filter { case (name, list) => list.exists(range => range.contains(value)) }
      .map { case (name, _) => name }
      .toSet
  }

  def decodeFields(tickets: List[String]) : Map[String, Int] = {
    var track = Map[Int, Set[String]]()

    tickets.foreach(ticket => {
      val fields = ticket
        .split(",")
        .map(_.toInt)
        .zipWithIndex

      fields.foreach { case (field, index) =>
        val decode = validFields(field)
        if (track.contains(index)) {
          track = track + (index -> (track(index).intersect(decode)))
        } else {
          track += (index -> decode)
        }
      }
    })

    while(track.values.exists(_.size > 1)) {
      val singles = track.filter { case (index, set) => set.size == 1 }.toMap

      track = track
        .map { case (index, set) =>
          if (singles.contains(index)) {
            (index -> set)
          } else {
            val result = singles.values.foldLeft(set) { case (set, single) => set -- single }
            (index -> result)
          }
        }
        .toMap
    }
    track.map { case (index, set) => (set.toIterator.next() -> index) }.toMap
  }
}

object RuleSet {
  def apply(lines: Iterator[String]) : RuleSet = {
    val ranges = lines
      .map((line) => {
        val parts = line.split(": ")
        val name = parts(0)
        val allowed = parts(1)
          .split(" or ")
          .map(rest => {
            val r = rest.split("-").map(_.toInt)
            Range(r(0), r(1)).inclusive
          })
          .toList
        (name -> allowed)
      })
      .toMap

    new RuleSet(ranges)
  }
}
