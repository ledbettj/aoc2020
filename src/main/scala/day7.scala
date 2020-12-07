package day7

import scala.collection.mutable.Map
import scala.collection.immutable

case class Bag(adj: String, color: String) {
  override def toString : String = s"$adj $color"
}

class BagRules {
  // map of bag -> contained by
  var rules : Map[Bag, Set[Bag]] = Map()

  // map of bag -> contains
  var counts : Map[Bag, immutable.Map[Bag, Int]] = Map()

  def addRule(container: Bag, contains: Set[Bag]) = {
    contains.foreach((contained) => {
      if (!rules.contains(contained)) {
        rules(contained) = Set()
      }

      rules(contained) += container
    })
  }

  def addCount(container: Bag, contains: immutable.Map[Bag, Int]) = {
    counts(container) = contains
  }

  def eventuallyContain(bag: Bag) : Set[Bag] = {
    var result = Set[Bag]()

    if (rules.contains(bag)) {
      result = rules(bag)
      rules(bag).foreach(otherBag => {
        result = result | eventuallyContain(otherBag)
      })
    }
    result
  }

  def containsCount(container: Bag) : Long = {
    if (!counts.contains(container)) {
      return 0l
    }
    counts(container)
      .map { case (bag, num) =>  num + num * containsCount(bag) }
      .reduce((a, b) => a + b)
  }
}

object BagRules {
  val containerRegex = raw"^(\w+) (\w+) bags contain".r
  val containedRegex = raw"(\d+) (\w+) (\w+) bag(s?)[,.]".r

  def apply(lines: Iterator[String]) : BagRules = {
    var rules = new BagRules()
    lines.foreach(line => {
      var container = containerRegex
        .findFirstMatchIn(line)
        .map(m => Bag(m.group(1), m.group(2)))
        .get

      var contains = containedRegex
        .findAllMatchIn(line)
        .map(m => Bag(m.group(2), m.group(3)))
        .toSet

      rules.addRule(container, contains)

      var cnts = containedRegex
        .findAllMatchIn(line)
        .map(m => (Bag(m.group(2), m.group(3)) -> m.group(1).toInt))
        .toMap

      if (!cnts.isEmpty)
        rules.addCount(container, cnts)
    })

    rules
  }
}

class Day7(input: Iterator[String]) {
  val rules = BagRules(input)

}
