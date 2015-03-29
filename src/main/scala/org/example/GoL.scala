package org.example

import scala.collection.immutable.IndexedSeq

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

  def tick(c: Set[(Int, Int)]): Set[(Int, Int)] = {

    // checking the entire field between min and max x & y
    // TODO - use the neighborhood of all cells - would it be more practical?
    var deceased: Set[(Int, Int)] = Set()
    var born: Set[(Int, Int)] = Set()
    locationsToCheck(c) foreach { (l) =>
      rules(countNeighbors(c, l._1, l._2)) match {
        case None =>
        case Some(v: Symbol) => if ('dead == v) deceased += l else born += l
      }
    }
    c -- deceased ++ born
  }

  def locationsToCheck(c: Set[(Int, Int)]): Set[(Int, Int)] = {
    val allX = c.map {_._1}
    val allY = c.map {_._2}
    (for {
      x <- (allX.min to allX.max)
      y <- (allY.min to allY.max)
    } yield (x -> y)).toSet
  }
}
