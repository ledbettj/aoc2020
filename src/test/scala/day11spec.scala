package day11

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List(
    "L.LL.LL.LL",
    "LLLLLLL.LL",
    "L.L.L..L..",
    "LLLL.LL.LL",
    "L.LL.LL.LL",
    "L.LLLLL.LL",
    "..L.L.....",
    "LLLLLLLLLL",
    "L.LLLLLL.L",
    "L.LLLLL.LL"
  )

  "Part1" should "work for sample" in {
    var p = SeatLayout.parse(sampleInput.toIterator)

    var n = p.step()
    while(!n.isEmpty) {
      p = n.get

      n = p.step()
    }
    p.occupiedCount() shouldEqual 37
  }

  "Part2" should "work for sample" in {
    var p = SeatLayout.parse(sampleInput.toIterator)
    var n = p.step2
    while(!n.isEmpty) {
      p = n.get

      n = p.step2()
    }
    p.occupiedCount() shouldEqual 26
  }



  "Part1" should "work for input" in {
    var p = SeatLayout.parse(InputLines.forDay(11))

    var n = p.step()
    while(!n.isEmpty) {
      p = n.get
      n = p.step()
    }
    p.occupiedCount() shouldEqual 2166
  }

  "Part2" should "work for input" in {
    var p = SeatLayout.parse(InputLines.forDay(11))

    var n = p.step2()
    while(!n.isEmpty) {
      p = n.get
      n = p.step2()
    }
    p.occupiedCount() shouldEqual 1955
  }

}
