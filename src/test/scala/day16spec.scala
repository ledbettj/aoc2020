package day16

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List(
    "class: 1-3 or 5-7",
    "row: 6-11 or 33-44",
    "seat: 13-40 or 45-50",
    "",
    "your ticket:",
    "7,1,14",
    "",
    "nearby tickets:",
    "7,3,47",
    "40,4,50",
    "55,2,20",
    "38,6,12"
  )

  // "Part1" should "work for sample input" in {
  //   val (r, _, tickets) = InputSplitter.split(sampleInput.toIterator)
  //   val rules = RuleSet(r.toIterator)
  //   val invalid = tickets.flatMap(ticket => rules.invalidFields(ticket))
  //   invalid.sum shouldEqual 71
  // }

  // "Part1" should "work for real input" in {
  //   val (r, _, tickets) = InputSplitter.split(InputLines.forDay(16))
  //   val rules = RuleSet(r.toIterator)
  //   val invalid = tickets.flatMap(ticket => rules.invalidFields(ticket))
  //   invalid.sum shouldEqual 23122
  // }

  "Part2" should "work for sample input" in {
    val sample = List(
      "class: 0-1 or 4-19",
      "row: 0-5 or 8-19",
      "seat: 0-13 or 16-19",
      "",
      "your ticket:",
      "11,12,13",
      "",
      "nearby tickets:",
      "3,9,18",
      "15,1,5",
      "5,14,9"
    )
    val (r, m, tickets) = InputSplitter.split(sample.toIterator)
    val rules = RuleSet(r.toIterator)
    var mine = m(0).split(",").map(_.toInt).toList
    val valid = tickets.filter(ticket => rules.isValid(ticket))
    val map = rules.decodeFields(valid)
    val answer = map
//      .filter { case (name, index) => name.startsWith("departure") }
      .map { case (_, index) => mine(index) }
      .reduce((a, b) => a * b)

    answer shouldEqual 12 * 11 * 13
  }

  "Part2" should "work for real input" in {
    val (r, m, tickets) = InputSplitter.split(InputLines.forDay(16))
    val rules = RuleSet(r.toIterator)
    var mine = m(0).split(",").map(_.toInt)
    val valid = tickets.filter(ticket => rules.isValid(ticket))
    val map = rules.decodeFields(valid)
    val answer = map
      .filter { case (name, index) => name.startsWith("departure") }
      .map { case (_, index) => mine(index).toLong }
      .reduce((a, b) => a.toLong * b.toLong)


    answer shouldEqual 362974212989L
  }

}
