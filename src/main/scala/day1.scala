package day1

import scala.collection.Set;

trait TwoSum {
  val Entries : Set[Int]

  /** Given a target value, return the first pair of integers in Entries which sum to the target.
    * This code does not correctly handle the case where a number sums with itself, i.e
    *  Entries = [1], target = 2
    *  But that seems to be OK since the problem doesn't require it.
    */
  def twoSum(target: Int) : Option[(Int, Int)] = {
    Entries
      .find(value => Entries.contains(target - value))
      .map(v => (v, target - v))
  }
}

trait ThreeSum extends TwoSum {
  /** Given a target value, return the first set of 3 integers in Entries which sum to the target.
    * This code does not correctly handle the case where a number sums with itself, i.e
    *  Entries = [1], target = 3
    *  But that seems to be OK since the problem doesn't require it.
    */
  def threeSum(target: Int) : Option[(Int, Int, Int)] = {
      Entries
        .map(value => twoSum(target - value))
        .find(result => !result.isEmpty)
        .flatten
        .map(t => (t._1, t._2, target - t._1 - t._2))
  }
}

class Day1(input: Iterator[String]) {
  lazy val Entries = Set.from(input.map(_.toInt))
}

class Part1(input: Iterator[String]) extends Day1(input) with TwoSum {
  def answer : Option[Int] = {
    twoSum(2020).map(x => x._1 * x._2)
  }
}

class Part2(input: Iterator[String]) extends Day1(input) with ThreeSum {
  def answer : Option[Int] = {
    threeSum(2020).map(x => x._1 * x._2 * x._3)
  }
}
