package day10

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List(
    "16",
    "10",
    "15",
    "5",
    "1",
    "11",
    "7",
    "19",
    "6",
    "12",
    "4"
  )

  "Part1" should "work for sample input" in {
    val x = new Jolty(sampleInput.toIterator)
    x.joltyDiff() shouldEqual Map(1 -> 7, 3 -> 5)
  }

  "Part1" should "work for real input" in {
    val x = new Jolty(InputLines.forDay(10))
    x.joltyDiff() shouldEqual Map(1 -> 70, 3 -> 27)
  }


  "Part2" should "work for sample input" in {
    val x = new Jolty(sampleInput.toIterator)
    x.validWays shouldEqual 8
  }

  "Part2" should "work for real input" in {
    val x = new Jolty(InputLines.forDay(10))
    x.validWays shouldEqual 49607173328384L
  }


}
