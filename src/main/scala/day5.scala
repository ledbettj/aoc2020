package day5

class Day5(input: Iterator[String]) {
  val seats = input.map(codeToSeatId).toSet

  private def codeToSeatId(encoded: String) : Int = encoded
    .map { case 'B' | 'R' => 1 ; case _ => 0 }
    .foldLeft(0) { case (result, bit) => result << 1 | bit }

  // Part 1
  def maxSeatId : Int = seats.max
  // Part2
  def mySeatId : Option[Int] = (0 until 1024).find { id => !seats(id) && seats(id + 1) && seats(id - 1) }
}
