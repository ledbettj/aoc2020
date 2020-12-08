package day7

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import util.InputLines

class Day7Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List(
    "light red bags contain 1 bright white bag, 2 muted yellow bags.",
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
    "bright white bags contain 1 shiny gold bag.",
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
    "faded blue bags contain no other bags.",
    "dotted black bags contain no other bags."
  )

  "Part1" should "work for sample" in {
    val target = Bag("shiny", "gold")
    val d7 = new Day7(sampleInput.toIterator)
    val set = d7.rules.eventuallyContain(target)
    println(s"set is $set")
    d7.rules.eventuallyContain(target).size shouldEqual 4

  }
  "Part1" should "calculateResult" in {
    val target = Bag("shiny", "gold")

    val d7 = new Day7(InputLines.forDay(7))
    d7.rules.eventuallyContain(target).size shouldEqual 278

  }

  val sample2 = List(
    "shiny gold bags contain 2 dark red bags.",
    "dark red bags contain 2 dark orange bags.",
    "dark orange bags contain 2 dark yellow bags.",
    "dark yellow bags contain 2 dark green bags.",
    "dark green bags contain 2 dark blue bags.",
    "dark blue bags contain 2 dark violet bags.",
    "dark violet bags contain no other bags."
  )
  "Part2" should "work for sample" in {
    val target = Bag("shiny", "gold")
    val d7 = new Day7(sample2.toIterator)

    d7.rules.containsCount(target) shouldEqual 126

  }

  "Part2" should "calculate result" in {
    val target = Bag("shiny", "gold")
    val d7 = new Day7(InputLines.forDay(7))

    d7.rules.containsCount(target) shouldEqual 45157

  }
}
