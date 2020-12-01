package day1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {
  "Part1" should "calculate result" in {
    val result = Part1.twoSum(2020)
    result shouldEqual Some((855, 1165))
    (855 * 1165) shouldEqual (996075)
  }

  "Part2" should "calculate result" in {
    val result = Part2.threeSum(2020)
    result shouldEqual Some((1667, 168, 185))
    (1667 * 168 * 185) shouldEqual  51810360
  }
}
