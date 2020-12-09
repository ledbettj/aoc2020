package day9

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day9Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List("35", "20", "15", "25", "47", "40", "62", "55", "65",
    "95", "102", "117", "150", "182", "127", "219", "299", "277", "309", "576")

  "Part1" should "work for sample input" in {
    val x = new XmsValidator(sampleInput.toIterator, 5)
    x.findFirstInvalid() shouldEqual Some(127)
  }

  "Part2" should "work for sample input" in {
    val x = new XmsValidator(sampleInput.toIterator, 5)
    var r = x.sequenceSumsTo(127).get
    (r.min + r.max) shouldEqual 62
  }


  "Part1" should "work for real" in {
    val x = new XmsValidator(InputLines.forDay(9), 25)
    x.findFirstInvalid() shouldEqual Some(36845998)
  }

  "Part2" should "work for real" in {
    val x = new XmsValidator(InputLines.forDay(9), 25)
    var r = x.sequenceSumsTo(36845998).get
    (r.min + r.max) shouldEqual 4830226
  }


}
