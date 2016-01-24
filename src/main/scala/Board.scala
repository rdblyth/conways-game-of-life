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
    val numLiveNeighbors = getNumberOfAliveNeighbors(cell)
    if (isAlive(cell)) {
      numLiveNeighbors == 2 || numLiveNeighbors == 3
    }
    else {
      numLiveNeighbors == 3
    }
  }

  private def getAliveCellsWithNeighbors() : Set[Cell] = {
    aliveCells.flatMap(cell => cell.neighbors) ++ aliveCells
  }

  private def getNumberOfAliveNeighbors(cell:Cell) : Int = {
    cell.neighbors.filter(cell => isAlive(cell)).length
  }
}