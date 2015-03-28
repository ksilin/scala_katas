package org.example

import scala.collection.immutable.IndexedSeq

object GoL {

  val neighborOffsets: IndexedSeq[(Int, Int)] = for {
    x <- (-1 to 1)
    y <- (-1 to 1)
    if (x != 0 || y != 0)
  } yield (x -> y)

  def nextState(i: Int): Option[Symbol] = {
    if (3 == i) Some('alive) else if (2 == i) None else Some('dead)
  }

  def countNeighbors(c: Set[(Int, Int)], x: Int, y: Int): Int = {
    val n = neighborIndices(x, y)
    n.filter(c.contains(_)).length
  }

  def neighborIndices(x: Int, y: Int): IndexedSeq[(Int, Int)] = {
    neighborOffsets map {(a) => ((a._1 + x) -> (a._2 + y))}
  }

  def max(xs: List[Int]): Option[Int] = xs match {
    case Nil => None
    case List(x: Int) => Some(x)
    case x :: y :: rest => max( (if (x > y) x else y) :: rest )
  }

  def minX(c: Set[(Int, Int)]): Int = {
    c.map{(a) => a._1}.min
  }

  def maxX(c: Set[(Int, Int)]): Int = {
    c.map{(a) => a._1}.max
  }

  def minY(c: Set[(Int, Int)]): Int = {
    c.map{(a) => a._2}.min
  }

  def maxY(c: Set[(Int, Int)]): Int = {
    c.map{(a) => a._2}.max
  }

  def tick(c: Set[(Int, Int)]): Set[(Int, Int)] = {

    // check the entire field between min and max x & y
    var cc = c
    var next: Option[Symbol] = None
    for( x <- (minX(c) to maxX(c)).toList; y <- (minY(c) to maxY(c)).toList) {

      next = nextState(countNeighbors(c, x, y))
      cc = next match {
        case None => cc
        case Some(v: Symbol) => if('dead == v) cc - (x -> y) else cc + (x -> y)
      }
    }
    cc
  }

  def isAlive(c: Set[(Int, Int)], x: Int, y: Int): Boolean = {
    c.contains(x -> y)
  }

}
