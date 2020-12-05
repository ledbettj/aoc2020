package day5

class BoardingPass(encoding: String) {
  val Rows = 128
  val Cols = 8

  val row = Integer.parseInt(encoding
    .substring(0, 7)
    .map(ch => ch match {
      case 'B' => 1
      case _ => 0
    })
    .map(_.toString)
    .mkString, 2)

  val col = Integer.parseInt(encoding
    .substring(7)
    .map(ch => ch match {
      case 'R' => 1
      case _ => 0
    })
    .map(_.toString)
    .mkString, 2)

  val seatId = row * 8 + col
}

class Day5(input: Iterator[String]) {
  val passes = input.map(new BoardingPass(_)).toList

}
