case class Cell(x:Int, y:Int) {

  lazy val neighbors : List[Cell] = {
    val neighbors = {
      for(i <- -1 to 1;
          j <- -1 to 1) yield Cell(x+i, y+j)
    }
    neighbors.filter(cell => cell != this).toList
  }
}
