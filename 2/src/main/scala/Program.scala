import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Program {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/resources/input").getLines()

    var currentPositionOne = Position(0,0,0)
    var currentPositionTwo = Position(0,0,0)

    // map gets a function as a param
    lines.map { line =>
      val tokens = line.split(" ").toList
      tokens match {
        case direction :: value :: Nil =>
          Success(Command(direction, value.toInt))
        case invalidLine => Failure(new IllegalArgumentException(s"The input is broken.\n$invalidLine"))
      }
    }.foreach { command =>
      currentPositionOne = processCommand(command.get,currentPositionOne)
      currentPositionTwo = processCommandTwo(command.get,currentPositionTwo)
    }

    println(currentPositionOne)
    println(currentPositionOne.horizontal * currentPositionOne.depth)

    println(currentPositionTwo)
    println(currentPositionTwo.horizontal * currentPositionTwo.depth)

    def processCommand(command: Command, currentPosition: Position): Position = {
      if("forward".equals(command.direction)){
        Position(currentPosition.horizontal+command.value,currentPosition.depth,0)
      } else if("down".equals(command.direction)) {
        Position(currentPosition.horizontal,currentPosition.depth+command.value,0)
      } else{
        Position(currentPosition.horizontal,currentPosition.depth-command.value,0)
      }
    }

    def processCommandTwo(command: Command, currentPosition: Position): Position = {
      if("forward".equals(command.direction)){
        Position(currentPosition.horizontal + command.value, currentPosition.depth + currentPosition.aim * command.value, currentPosition.aim)
      } else if("down".equals(command.direction)) {
        Position(currentPosition.horizontal, currentPosition.depth, currentPosition.aim + command.value)
      } else{
        Position(currentPosition.horizontal, currentPosition.depth, currentPosition.aim - command.value)
      }
    }

  }
}
