package day5

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day5Spec extends AnyFlatSpec with Matchers {
  "Part1" should "calculate solution" in {
    val day5 = new Day5(InputLines.forDay(5))
    day5.maxSeatId shouldEqual 878
  }

  "Part2" should "calculate solution" in {
    val day5 = new Day5(InputLines.forDay(5))
    day5.mySeatId shouldEqual Some(504)
  }
}
