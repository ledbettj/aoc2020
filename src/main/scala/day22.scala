package day22

import scala.collection.immutable.Queue

case class RecursiveCombat(val player1: Queue[Long], val player2: Queue[Long],
  history: Set[(Queue[Long], Queue[Long])]) {
  def turn() : RecursiveCombat = {
    var p1winner = false
    if (history.contains((player1, player2))) {
      // game over p1 win
      p1winner = true
      throw new Exception("Game ovah")
    }

    var (p1card, next1) = player1.dequeue
    var (p2card, next2) = player2.dequeue

    if (p1card <= next1.size && p2card <= next2.size) {


    } else {
      p1winner = p1card > p2card
    }


    if (p1winner) {
      next1 = next1.enqueueAll(List(p1card, p2card))
    } else {
      next2 = next1.enqueueAll(List(p1card, p2card))
    }

    val nextHistory = history + ((player1, player2))
    RecursiveCombat(next1, next2, nextHistory)
  }

  // def play(): (RecursiveCombat, Int) = {
    

  // }
}

case class Combat(val player1: Queue[Long], val player2: Queue[Long]) {

  def turn() : Combat = {
    var (p1card, next1) = player1.dequeue
    var (p2card, next2) = player2.dequeue

    if (p1card > p2card) {
      next1 = next1.enqueueAll(List(p1card, p2card))
    } else {
      next2 = next2.enqueueAll(List(p2card, p1card))
    }

    Combat(next1, next2)
  }

  def isGameOver() = player1.isEmpty || player2.isEmpty

  def score(player: Queue[Long]) = {
    val len = player.size

    player
      .zipWithIndex
      .map { case (card, index) => card * (len - index).toLong }
      .sum
  }

  def winnerScore = if (player1.isEmpty) score(player2) else score(player1)
}

object Combat {
  def parse(input: List[String]) : Combat = {
    val middle = input.indexOf("")
    val (p1, p2) = input.splitAt(middle)

    var player1 = p1.drop(1).map(_.toLong)
    var player2 = p2.drop(2).map(_.toLong)

    Combat(Queue(player1: _*), Queue(player2: _*))
  }
}
