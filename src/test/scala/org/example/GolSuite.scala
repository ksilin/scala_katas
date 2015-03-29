package org.example

import org.scalatest.FunSuite

import org.example.GoL._

class GolSuite extends FunSuite {

  test("death by overcrowding")(assert('dead == rules(4).get))
  test("birth")(assert('alive == rules(3).get))
  test("stable")(assert(None == rules(2)))
  test("death by loneliness")(assert('dead == rules(1).get))
  test("death by loneliness 2")(assert('dead == rules(0).get))

  // counting neighbors

  test("creating a neighbor set") {
    val indices = neighborIndices(1, 2)
    println(indices.mkString(","))
  }

  test("a single cell has no neighbors") {
    val cells: Set[(Int, Int)] = Set((1, 1))
    val neighbors = countNeighbors(cells, 1, 1)
    assert(0 == neighbors)
  }

  test("two neighboring cells know each other") {
    val cells: Set[(Int, Int)] = Set((1, 1), (1, 2))
    assert(1 == countNeighbors(cells, 1, 1))
    assert(1 == countNeighbors(cells, 1, 2))
  }

  test("each cell in a 2x2 square has 3 neighbors") {
    val cells: Set[(Int, Int)] = Set((1, 1), (1, 2), (2, 1), (2, 2))
    assert(3 == countNeighbors(cells, 1, 1))
    assert(3 == countNeighbors(cells, 1, 2))
    assert(3 == countNeighbors(cells, 2, 1))
    assert(3 == countNeighbors(cells, 2, 2))
  }

  // tick

  test("single cell dies on tick"){
    var cells: Set[(Int, Int)] = Set((1,1))
    cells = tick(cells)
    assert(!cells.contains(1 -> 1))
  }

  test("a 2x2 square survives on tick"){
    var cells: Set[(Int, Int)] = Set((1, 1), (1, 2), (2, 1), (2, 2))
    cells = tick(cells)
    assert(cells.contains(1 -> 1))
    assert(cells.contains(1 -> 2))
    assert(cells.contains(2 -> 1))
    assert(cells.contains(2 -> 2))
  }

}
