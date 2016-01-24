import scala.collection.mutable.MutableList

case class Cell(x:Int, y:Int) {

  def neighbors : List[Cell] = {
    val neighbors = MutableList[Cell]()
    for(i <- -1 to 1;
        j <- -1 to 1) {
      neighbors +=  Cell(x+i, y+j)
    }
    neighbors.filter(cell => cell != this).toList
  }
}
