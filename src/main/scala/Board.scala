case class Board(aliveCells: Set[Cell]) {

  def tick() : Board = new Board(aliveCellsAfterTick())

  def isAlive(cell:Cell) : Boolean = aliveCells.contains(cell)

  private def aliveCellsAfterTick() = aliveCellsWithNeighbors().filter(isAliveAfterTick)

  private def isAliveAfterTick(cell:Cell)  = {
    (numAliveNeighbors(cell), isAlive(cell)) match {
      case (2, true) => true
      case (3, _) => true
      case _ => false
    }
  }

  private def aliveCellsWithNeighbors() = aliveCells ++ aliveCells.flatMap(_.neighbors)

  private def numAliveNeighbors(cell:Cell) = cell.neighbors.filter(isAlive).length
}