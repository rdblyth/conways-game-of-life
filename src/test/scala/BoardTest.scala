import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class BoardTest extends FunSuite with Checkers {
  test("Cell is alive") {
    val board = new Board(List(Cell(0, 0)))
    assert(board.isAlive(Cell(0, 0)) )
  }

  test("Cell is dead") {
    val board = new Board(List(Cell(0, 0)))
    assert(!board.isAlive(Cell(1, 0)) )
  }

  test("Cell has 8 neighbors") {
    assert(Cell(0,0).neighbors.length == 8)
  }

  test("Cell neighbors") {
    val cell = Cell(0,0)
    assert(cell.neighbors.contains(Cell(0,1)))
    assert(cell.neighbors.contains(Cell(0,-1)))
    assert(cell.neighbors.contains(Cell(1,0)))
    assert(cell.neighbors.contains(Cell(1,1)))
    assert(cell.neighbors.contains(Cell(1,-1)))
    assert(cell.neighbors.contains(Cell(-1,0)))
    assert(cell.neighbors.contains(Cell(-1,1)))
    assert(cell.neighbors.contains(Cell(-1,-1)))
  }

  test("Cell with fewer than two live neighbors dies") {
    val board = new Board(List(Cell(0,0)))
    assert(!board.tick().isAlive(Cell(0,0)))
  }

  test("Alive cell with two live neighbors lives on") {
    val board = new Board(List(Cell(0,0), Cell(1,0), Cell(-1,0)))
    assert(board.tick().isAlive(Cell(0,0)))
  }

  test("Alive cell with three live neighbors lives on") {
    val board = new Board(List(Cell(0,0), Cell(1,0), Cell(-1,0), Cell(0,1)))
    assert(board.tick().isAlive(Cell(0,0)))
  }

  test("Alive cell with more than three live neighbors dies") {
    val board = new Board(List(Cell(0,0), Cell(1,0), Cell(-1,0), Cell(0,1), Cell(0, -1)))
    assert(!board.tick().isAlive(Cell(0,0)))
  }

  test("Dead cell with exact three live neighbors becomes a live") {
    val board = new Board(List(Cell(1,0), Cell(-1,0), Cell(0,1)))
    assert(board.tick().isAlive(Cell(0,0)))
  }

  test("Dead cell with less than three live neighbors stays dead") {
    val board = new Board(List(Cell(1,0), Cell(-1,0)))
    assert(!board.tick().isAlive(Cell(0,0)))
  }

  test("Dead cell with more than three live neighbors stays dead") {
    val board = new Board(List(Cell(1,0), Cell(-1,0), Cell(0,1), Cell(0, -1)))
    assert(!board.tick().isAlive(Cell(0,0)))
  }
}
