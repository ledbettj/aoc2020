package day9

import scala.collection.mutable.Map

class XmsValidator(input: Iterator[String], val windowSize: Int) {
  val numbers = input.map(_.toLong).toIndexedSeq
  val window : Map[Long, Int] = numbers
    .take(windowSize)
    .foldLeft(Map[Long, Int]()){ case (map, n) =>
      val count = map.getOrElse(n, 0)
      map(n) = count  + 1
      map
    }

  def isValid(target: Long) : Boolean = {
    window
      .exists { case (num, count) =>
        val other = target - num
        window.getOrElse(other, 0) > 0 && (other != num || count > 1)
      }
  }

  def findFirstInvalid() : Option[Long] = {
    numbers
      .zipWithIndex
      .drop(windowSize)
      .find { case (number, index) =>
        val result = isValid(number)
        shiftWindow(index)
        !result
      }
    .map { case (number, index) => number }
  }

  def addNumber(n: Long) = {
    val count = window.getOrElse(n, 0)
    window(n) = count  + 1
  }

  def removeNumber(n: Long) = {
    val count = window.get(n).get - 1
    if (count > 0) {
      window(n) = count
    } else {
      window.remove(n)
    }
  }

  def shiftWindow(index: Int) = {
    val curNum = numbers(index - windowSize)
    removeNumber(curNum)
    addNumber(numbers(index))
  }

  def sequenceSumsTo(target: Long) : Option[IndexedSeq[Long]] = {
    var sum = 0l

    for (index <- (0 until numbers.length)) {
      sum = 0l;
      val result = numbers.drop(index).takeWhile(n => { sum += n ; sum <= target })
      if (result.sum == target) {
        return Some(result)
      }
    }

    None
  }
}
