package day23

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day23Spec extends AnyFlatSpec with Matchers {
  val sample = Vector(3, 8, 9, 1, 2, 5, 4, 6, 7)

  "Part1" should "work for sample" in {
    var cups = CupRing(sample, 3)

    for (i <- 0 until 10) {
      cups = cups.turn()
    }
    cups.state shouldEqual "583741926"
  }

  "Part1" should "work for 100 sample" in {
    var cups = CupRing(sample, 3)

    for (i <- 0 until 100) {
      cups = cups.turn()
    }
    cups.state shouldEqual "291673845"
  }


  "Part1" should "work for real" in {
    var cups = CupRing(Vector(5, 8, 9, 1, 7, 4, 2, 6, 3), 5)

    for (i <- 0 until 100) {
      cups = cups.turn()
    }
    cups.state shouldEqual "514389672"
    // 43896725
  }

  "Part2" should "work for sample" in {
    val start = sample.max + 1
    var ext = sample ++ (start to 1_000_000).toVector
    var cups = CupRing(ext, 3)
    var seen = scala.collection.mutable.Map[CupRing, Int]()
    for (i <- 0 until 10_000_000) {
      seen(cups) = i
      cups = cups.turn()
      if (seen.contains(cups)) {
        println(s"after turn $i cups are in arrangement from turn ${seen(cups)}")
        throw new Exception("Bailz")
      } 
    }

  }
}
