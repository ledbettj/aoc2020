package day1
import scala.collection.Set;
import util.WithInput;

trait TwoSum {
  val Entries : Set[Int]

  /** Given a target value, return the first set of 2 integers in Entries which sum to the target.
    * This code does not correctly handle the case where a number sums with itself, i.e
    *  Entries = [1], target = 2
    *  But that seems to be OK since the problem doesn't require it.
    */
  def twoSum(target: Int) : Option[(Int, Int)] = {
    val one = Entries.find(value => Entries.contains(target - value))

    one match {
      case None => None
      case Some(v) => Some(v, target - v)
    }
  }
}

object Part1 extends TwoSum with WithInput {
  val DayNumber = 1
  lazy val Entries = Set.from(Input.map(line => line.toInt))
 }

object Part2 extends TwoSum with WithInput {
  val DayNumber = 1
  lazy val Entries = Set.from(Input.map(line => line.toInt))

  /** Given a target value, return the first set of 3 integers in Entries which sum to the target.
    * This code does not correctly handle the case where a number sums with itself, i.e
    *  Entries = [1], target = 3
    *  But that seems to be OK since the problem doesn't require it.
    */
  def threeSum(target: Int) : Option[(Int, Int, Int)] = {
    val two =
      Entries
        .map((value) => {
          val subTarget = target - value
          this.twoSum(subTarget)
        })
        .find(result => !result.isEmpty)
        .flatten

    two match {
      case None => None
      case Some((a, b)) => Some((a, b, target - a - b))
    }
  }
}
