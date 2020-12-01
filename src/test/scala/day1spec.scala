package day1

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {
  "Part1" should "implement working 2-sum" in {
    val input = Array("1", "2", "3", "4")
    val p1 = new Part1(input.toIterator)
    p1.twoSum(7) shouldEqual Some(3, 4)
    p1.twoSum(9) shouldBe None
  }

  "Part2" should "implement working 3-sum" in {
    val input = Array("1", "2", "3", "4")
    val p2 = new Part2(input.toIterator)
    p2.threeSum(7) shouldEqual Some(2, 4, 1)
    p2.threeSum(14) shouldBe None
  }

  "Part1" should "calculate result" in {
    val p1 = new Part1(InputLines.forDay(1))
    p1.answer shouldEqual Some(996075)
  }

  "Part2" should "calculate result" in {
    val p2 = new Part2(InputLines.forDay(1))
    p2.answer shouldEqual Some(51810360)
  }
}
