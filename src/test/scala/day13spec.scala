package day13

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List(
    "939",
    "7,13,x,x,59,x,31,19",
  )

  "Part1" should "work for sample input" in {
    val x = new Part1(sampleInput.toIterator)
    x.nextDeparture() shouldEqual 59
    x.answer() shouldEqual 295
  }

  "Part1" should "work for real input" in {
    val x = new Part1(InputLines.forDay(13))
    x.nextDeparture() shouldEqual 787
    x.answer() shouldEqual 4722
  }

  "Part2" should "work for small sample input" in {
    val s = List("_", "17,x,13,19")
    val x = new Part2(s.toIterator)
    println(s"${x.busses.toList}")
    x.answer() shouldEqual 3417
  }

  "Part2" should "work for sample input" in {
    val x = new Part2(sampleInput.toIterator)
    println(s"${x.busses.toList}")
    x.answer() shouldEqual 1068781L
  }


  "Part2" should "work for real input" in {
    val x = new Part2(InputLines.forDay(13))
    println(s"${x.busses.toList}")
    x.answer() shouldEqual 825305207525452L
  }
}
