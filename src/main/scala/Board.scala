class Board(aliveCells:Set[Cell]) {

  def tick() : Board = {
    new Board(getAliveCellsAfterTick())
  }

  def isAlive(cell:Cell) : Boolean = {
    aliveCells.contains(cell)
  }

  private def getAliveCellsAfterTick() : Set[Cell] = {
    getAliveCellsWithNeighbors().filter(cell=> isAliveAfterTick(cell))
  }

  private def isAliveAfterTick(cell:Cell) : Boolean = {
    (isAlive(cell), getNumberOfAliveNeighbors(cell)) match {
      case (true, 2) => true
      case (_, 3) => true
      case _ => false
    }
  }

  private def getAliveCellsWithNeighbors() : Set[Cell] = {
    aliveCells.flatMap(cell => cell.neighbors) ++ aliveCells
  }

  private def getNumberOfAliveNeighbors(cell:Cell) : Int = {
    cell.neighbors.filter(cell => isAlive(cell)).length
  }
}