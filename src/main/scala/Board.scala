case class Board(aliveCells:Set[Cell]) {

  def tick() : Board = {
    new Board(getAliveCellsAfterTick())
  }

  def isAlive(cell:Cell) : Boolean = {
    aliveCells.contains(cell)
  }

  private def getAliveCellsAfterTick() : Set[Cell] = {
    getAliveCellsWithNeighbors().filter(isAliveAfterTick(_))
  }

  private def isAliveAfterTick(cell:Cell) : Boolean = {
    (getNumberOfAliveNeighbors(cell), isAlive(cell)) match {
      case (2, true) => true
      case (3, _) => true
      case _ => false
    }
  }

  private def getAliveCellsWithNeighbors() : Set[Cell] = {
    aliveCells ++ aliveCells.flatMap(_.neighbors)
  }

  private def getNumberOfAliveNeighbors(cell:Cell) : Int = {
    cell.neighbors.filter(cell => isAlive(cell)).length
  }
}