package org.example

import scala.collection.immutable.IndexedSeq
import scala.language.existentials

object GoL {

  val neighborOffsets: IndexedSeq[(Int, Int)] = for {
    x <- (-1 to 1)
    y <- (-1 to 1)
    if (x != 0 || y != 0)
  } yield (x -> y)

  val rules = Map(3 -> Some('alive), 2 -> None).withDefaultValue(Some('dead))

  def countNeighbors(c: Set[(Int, Int)], x: Int, y: Int): Int = {
    neighborIndices(x, y).filter(c.contains(_)).length
  }

  def neighborIndices(x: Int, y: Int): IndexedSeq[(Int, Int)] = {
    neighborOffsets map { (a) => ((a._1 + x) -> (a._2 + y)) }
  }

  def mapToLoc(e: ((Int, Int), Some[Symbol])): (Int, Int) = {
    e._1
  }

  def willBe(state: Symbol): PartialFunction[((Int, Int), Option[Symbol]), (Int, Int)] = {
//    def f(x: ((Int, Int), Option[Symbol])): (Int, Int) =
    {case p @ ((_, _), Some(state)) => p._1}
  }

  def tick(c: Set[(Int, Int)]): Set[(Int, Int)] = {
    val nextStates: Set[((Int, Int), Option[Symbol])] = locationsToCheck(c).map {
      (loc) => (loc -> rules(countNeighbors(c, loc._1, loc._2)))
    } filter (_._2.isDefined)
    // TODO - why does it work with the collect function inline
    // but the willBe function does not work properly
    val deceased = nextStates collect {case (loc: (Int, Int), Some('dead)) => loc}
    val born = nextStates collect {case (loc: (Int, Int), Some('alive)) => loc}
//    val born = nextStates collect willBe('alive)
    c -- deceased ++ born
  }

  // TODO - use the neighborhood of all cells - would it be more practical?
  def locationsToCheck(c: Set[(Int, Int)]): Set[(Int, Int)] = {
    val allX = c.map(_._1)
    val allY = c.map(_._2)
    (for {
      x <- (allX.min to allX.max)
      y <- (allY.min to allY.max)
    } yield (x -> y)).toSet
  }
}
