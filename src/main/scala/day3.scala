package day3

trait SlopeTraverser {
  val velocity : (Int, Int)

  def charAt(x: Int, y: Int) : Option[Char]

  def countTrees: Int = {
    var pos = (0, 0)
    var ch : Option[Char] = None
    var sum = 0

    while ({
      ch = charAt(pos._1, pos._2)
      !ch.isEmpty
    }) {
      sum += (ch match {
        case Some('#') => 1
        case Some(_) => 0
        case None => 0 // wont happen, is guarded in the loop above
      })
      pos = (pos._1 + velocity._1, pos._2 + velocity._2)
    }

    sum
  }
}

class Day3(input: Iterator[String]) {
  val grid : Vector[String] = input.toVector

  def charAt(x: Int, y: Int) : Option[Char] = {
    if (y >= grid.size) {
      None
    } else {
      val line = grid(y)
      Some(line.charAt(x % line.size))
    }
  }
}


class Part1(input: Iterator[String]) extends Day3(input) with SlopeTraverser {
  val velocity = (3, 1)
}

class Part2(input: Iterator[String], val velocity : (Int, Int)) extends Day3(input) with SlopeTraverser {

}
