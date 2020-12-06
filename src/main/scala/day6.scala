package day6

import scala.collection.mutable

class Group {
  var memberCount : Int = 0
  var items : mutable.Map[Int, Int] = mutable.Map()

  def processLine(line: String) = {
    memberCount += 1
    line.chars.forEach(ch => {
      if (items.contains(ch)) {
        items(ch) += 1
      } else {
        items(ch) = 1
      }
    })
  }

  def yesCount : Int = {
    items.count { case (key, value) => value == memberCount }
  }
}

class Day6(input: Iterator[String]) {
  var forms : List[mutable.Set[Char]] = input.foldLeft(List(mutable.Set[Char]()))((list, line) => {
    if (line.isBlank) {
      mutable.Set[Char]() :: list
    } else {
      line.chars.forEach(ch => list(0) += ch.toChar)
      list
    }
  })

  def yesCount : Int = forms.map(_.size).sum
}


class Part2(input: Iterator[String]) {
 var forms : List[Group] = input.foldLeft(List(new Group()))((list, line) => {
    if (line.isBlank) {
      new Group() :: list
    } else {
      list(0).processLine(line)
      list
    }
  })

  def yesCount : Int = forms.map(_.yesCount).sum
}
