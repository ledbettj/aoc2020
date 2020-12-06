package day6

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day6Spec extends AnyFlatSpec with Matchers {
  "Day6" should "solve p1" in {
    val d6 = new Day6(InputLines.forDay(6))
    d6.yesCount shouldEqual 6778
  }

  "Day6" should "solve p2" in {
    val d6 = new Part2(InputLines.forDay(6))
    d6.yesCount shouldEqual 3406
  }
}

