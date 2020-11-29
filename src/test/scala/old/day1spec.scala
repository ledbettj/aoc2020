package day1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1 extends AnyFlatSpec with Matchers {
  "Part1" should "calculate result" in {
    Part1.fuelRequired(300) shouldEqual 98
  }

  "Part1" should "find solution" in {
    Part1.totalFuelRequired(Part1.Masses) shouldEqual 3412496
  }

  "Part2" should "calculate result" in {
    Part2.recursiveFuelRequired(100756) shouldEqual 50346
  }

  "Part2" should "find solution" in {
    Part2.recursiveTotalFuelRequired(Part2.Masses) shouldEqual 5_115_845
  }
}
