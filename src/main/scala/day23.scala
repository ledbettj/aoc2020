package day23

case class CupRing(nums: Vector[Int], val current: Int) {

  def state = nums.map(_.toString).mkString
  def turn() = {
    // println("-- move -- ")
    // print("cups: ")
    // println(nums.map( { case `current` => s"($current)"
    //   case d => s"$d" }).mkString(" "))

    val pos = indexOfLabel(current)
    val pickedIndexes = ((pos + 1) to (pos + 3)).map(normalizeIndex _)
    val pickedLabels = pickedIndexes.map(index => labelAt(index)).toVector
    var destLabel = decrementLabel(current)
    // print("pick up: ")
    // println(pickedLabels.mkString(", "))

    while (pickedLabels.contains(destLabel)) {
      destLabel = decrementLabel(destLabel)
    }
    //println(s"destination: $destLabel\n")

    val destIndex = indexOfLabel(destLabel)
    var nextRing = (0 to destIndex)
      .filter(index => !pickedIndexes.contains(index))
      .map(index => labelAt(index))
      .toVector

    nextRing = nextRing ++ pickedLabels
    nextRing = nextRing ++ ((destIndex + 1) until nums.size)
      .filter(index => !pickedIndexes.contains(index))
      .map(index => labelAt(index))
      .toVector

    val tmp = CupRing(nextRing, 0)
    var nextCurrent = tmp.labelAt(tmp.indexOfLabel(current) + 1)
    val next = CupRing(nextRing, nextCurrent)
    next
  }

  def decrementLabel(cup: Int) = if (cup == 1) { nums.max  } else { cup - 1}

  def normalizeIndex(offset: Int) = {
    val off = if (offset < 0) nums.size - offset else offset
    off % nums.size
  }
  def indexOfLabel(cup : Int) = nums.indexOf(cup)
  def labelAt(offset: Int) = nums(normalizeIndex(offset))
}
