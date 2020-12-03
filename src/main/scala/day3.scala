package day3

class SlopeIterator(val Velocity : (Int, Int), grid: Vector[String]) extends Iterator[Char] {
  var pos = (0, 0)

  /** True if the next Y position is in bounds of the list. */
  def hasNext : Boolean = {
     nextPos._2 < grid.size
  }

  def next() : Char = {
    pos = nextPos
    charAt(pos)
  }

  private
  def charAt(p: (Int, Int)) : Char = {
    val (x, y) = p
    val line = grid(y)
    line.charAt(x % line.size)
  }

  private
  def nextPos : (Int, Int) = (pos._1 + Velocity._1, pos._2 + Velocity._2)
}

class Day3(input: Iterator[String], val velocity: (Int, Int)) {
  val grid : Vector[String] = input.toVector

  def toIterator : SlopeIterator = new SlopeIterator(velocity, grid)
  def countTrees : Int = toIterator.count(_ == '#')
}
