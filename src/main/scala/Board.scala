case class Board(aliveCells: Set[Cell]) {

  def tick() : Board = new Board(getAliveCellsAfterTick())

  def isAlive(cell:Cell) : Boolean = aliveCells.contains(cell)

  private def getAliveCellsAfterTick() = getAliveCellsWithNeighbors().filter(isAliveAfterTick)

  private def isAliveAfterTick(cell:Cell)  = {
    (getNumberOfAliveNeighbors(cell), isAlive(cell)) match {
      case (2, true) => true
      case (3, _) => true
      case _ => false
    }
  }

  private def getAliveCellsWithNeighbors() = aliveCells ++ aliveCells.flatMap(_.neighbors)

  private def getNumberOfAliveNeighbors(cell:Cell) = cell.neighbors.filter(isAlive).length
}