package day5

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day5Spec extends AnyFlatSpec with Matchers {
  "Day5" should "parse row/col" in {
    val b = new BoardingPass("FBFBBFFRLR")
    b.row shouldEqual 44
    b.col shouldEqual 5
  }

  "Part1" should "calculate solution" in {
    val day5 = new Day5(InputLines.forDay(5))
    day5.passes.maxBy(_.seatId).seatId shouldEqual 878
  }

  "Part2" should "calculate solution" in {
    val day5 = new Day5(InputLines.forDay(5))
    var seats = Array.ofDim[Boolean](128, 8)
    var ids = day5.passes.map(_.seatId).toSet

    day5.passes.foreach((pass) => {
      seats(pass.row)(pass.col) = true
    })

    val mine = seats
      .zipWithIndex
      .flatMap({ case (row, rowNum) =>
        row
          .zipWithIndex
          .filter({ case (isTaken, colNum) =>
            val id = rowNum * 8 + colNum
            !isTaken && ids.contains(id + 1) && ids.contains(id - 1)
          })
          .map(pair => (rowNum, pair._2))
      }).toList(0)

    mine shouldEqual (63, 0) // 63 * 8 + 0
  }

}
