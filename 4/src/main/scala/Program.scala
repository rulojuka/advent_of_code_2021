import scala.io.Source

object Program {

  def main(args: Array[String]): Unit = {
    val bufferedSource = Source.fromFile("src/main/resources/input")
    val lines = bufferedSource.getLines.toList

    val numbers = lines.head.split(",").map(_.toInt)
    var boards = Set.empty[Board]

    var index = 2
    while(lines.size > index){
      var numbersToInsert = Array.empty[(Int,Int,Int)]
      for(i <- 0 to 4){
        lines(index + i)
          .trim
          .split("\\s+")
          .map(_.toInt)
          .zipWithIndex
          .foreach{ tuple =>
             numbersToInsert = numbersToInsert :+ (tuple._1,i,tuple._2)
          }
      }
      boards = boards + new Board(numbersToInsert)
      index = index + 6
    }
    bufferedSource.close()

    // numbers.foreach(println)
    // boards.foreach( board => println(board.toString))

    var currentNumberIndex = 0
    while(boards.nonEmpty && currentNumberIndex<numbers.length){
      val currentMarkedNumber = numbers(currentNumberIndex)
      boards.foreach(board => board.mark(currentMarkedNumber))
      currentNumberIndex = currentNumberIndex + 1
      val winnerBoards = boards.collect{ case board if board.hasWon => board }
      if(winnerBoards.nonEmpty){
        winnerBoards.foreach{ winnerBoard =>
          val winningScore = winnerBoard.winningScore()
          // println(s"The winning number is ${currentMarkedNumber}")
          println(s"The winning score is ${winningScore}")
          // println(s"The board is ${winnerBoard}")
        }
        boards = boards &~ winnerBoards
      }
    }

    // The winning score is 44736
    // The winning score is 1827

  }
}