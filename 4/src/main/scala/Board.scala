class Board(val numbersWithCoordinates: Iterable[(Int,Int,Int)]) {

  private var lastMarkedNumber = -1
  private var markedNumbers = Set.empty : Set[Int]

  def mark(number: Int) : Unit = {
    markedNumbers = markedNumbers + number
    lastMarkedNumber = number
  }

  def hasWon: Boolean = {
    val markedElements = numbersWithCoordinates.filter(element => markedNumbers.contains(element._1))
    for(i <- 0 to 4){
      val inTheLine = markedElements.filter(element => element._2 == i)
      val inTheColumn = markedElements.filter(element => element._3 == i)
      if( inTheLine.size == 5 || inTheColumn.size == 5 ) {
        return true
      }
    }
    false
  }

  def winningScore(): Int = {
    val sumOfAllUnmarkedNumbers = numbersWithCoordinates
      .map(tuple => tuple._1)
      .filter(element => !markedNumbers.contains(element))
      .sum
    lastMarkedNumber * sumOfAllUnmarkedNumbers
  }

  override def toString: String = {
    numbersWithCoordinates.map{ numberWithCoordinate =>
      s"(${numberWithCoordinate._1}, ${numberWithCoordinate._2}, ${numberWithCoordinate._3})"
    }.reduce( (x,y) => s"${x}\n${y}")
  }
}
