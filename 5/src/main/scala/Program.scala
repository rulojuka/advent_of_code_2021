import scala.io.Source

object Program {

  def main(args: Array[String]): Unit = {
    val bufferedSource = Source.fromFile("src/main/resources/input")
    val lines = bufferedSource.getLines.toList
    val coordinates = lines.map{ line =>
      val tokens = ("[0-9]+".r findAllIn line).toList
      Line(tokens(0).toInt,tokens(1).toInt,tokens(2).toInt,tokens(3).toInt)
    }.toSet
    bufferedSource.close()

    val MAX = 1000
    val board: Array[Array[Int]] = Array.tabulate(MAX,MAX)( (x,y) => 0 )

    val horizontalLines = coordinates.collect{ case line if line.isHorizontal => line }
    val verticalLines = coordinates.collect{ case line if line.isVertical => line }
    val diagonalOneLines = coordinates.collect{ case line if line.isDiagonalOne => line }
    val diagonalTwoLines = coordinates.collect{ case line if line.isDiagonalTwo => line }

    horizontalLines.foreach{ line =>
      for( j <- line.x1 to line.x2){
        board(line.y1)(j) = board(line.y1)(j) + 1
      }
    }
    verticalLines.foreach{ line =>
      for( i <- line.y1 to line.y2){
        board(i)(line.x1) = board(i)(line.x1) + 1
      }
    }

    val numberOfElementsGreaterOrEqualToTwo = board.flatten.count(number => number >= 2)
    println(s"Elements: ${numberOfElementsGreaterOrEqualToTwo}") // 4745

    diagonalOneLines.foreach{ line =>
      for( i <- 0 to (line.x2 - line.x1)){
        board(line.y1+i)(line.x1+i) = board(line.y1+i)(line.x1+i) + 1
      }
    }
    diagonalTwoLines.foreach{ line =>
      for( i <- 0 to (line.x2 - line.x1)){
        board(line.y1-i)(line.x1+i) = board(line.y1-i)(line.x1+i) + 1
      }
    }

    val numberOfElementsGreaterOrEqualToTwoAgain = board.flatten.count(number => number >= 2)
    println(s"Elements: ${numberOfElementsGreaterOrEqualToTwoAgain}") // 18442

  }
}