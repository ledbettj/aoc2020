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

  "Part1" should "calculate solution" in {
    val p1 = new Day3(InputLines.forDay(3), (3, 1))

    p1.countTrees shouldEqual 223
  }

  "Part2" should "work for samples" in {
    val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    val solution = slopes
      .map(velocity => new Day3(sampleInput.toIterator, velocity))
      .map(_.countTrees)
      .reduce((a, b) => a * b)

    solution shouldEqual 336
  }

  "Part2" should "calculate solution" in {
    val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

    val solution = slopes
      .map(velocity => new Day3(InputLines.forDay(3), velocity))
      .map(_.countTrees.toLong)
      .reduce((a, b) => a * b)

    solution shouldEqual 3_517_401_300l
  }
}
