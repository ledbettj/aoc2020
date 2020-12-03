package day3

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Spec extends AnyFlatSpec with Matchers {
  val sampleInput = List(
    "..##.........##.........##.........##.........##.........##.......",
    "#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..",
    ".#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.",
    "..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#",
    ".#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.",
    "..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....",
    ".#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#",
    ".#........#.#........#.#........#.#........#.#........#.#........#",
    "#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...",
    "#...##....##...##....##...##....##...##....##...##....##...##....#",
    ".#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#"
  )


  "Day3" should "load the map correctly" in {
    val items = List("#..", ".#.", "..#").toIterator
    val day3 = new Day3(items)

    day3.charAt(0, 0) shouldEqual Some('#')
    day3.charAt(3, 0) shouldEqual Some('#')

    day3.charAt(1, 1) shouldEqual Some('#')
    day3.charAt(2, 2) shouldEqual Some('#')

    day3.charAt(1, 3) shouldEqual None
  }

  "Part1" should "calculate solution" in {
    val p1 = new Part1(InputLines.forDay(3))

    p1.countTrees shouldEqual 223

  }

  "Part2" should "work for samples" in {
    val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    val solution = slopes
      .map(velocity => new Part2(sampleInput.toIterator, velocity))
      .map(_.countTrees)
      .reduce((a, b) => a * b)

    solution shouldEqual 336
  }

  "Part2" should "calculate solution" in {
    val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

    val solution = slopes
      .map(velocity => new Part2(InputLines.forDay(3), velocity))
      .map(_.countTrees.toLong)
      .reduce((a, b) => a * b)

    solution shouldEqual 3_517_401_300l
  }
}
