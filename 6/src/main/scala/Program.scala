import scala.io.Source

object Program {

  def main(args: Array[String]): Unit = {
    val bufferedSource = Source.fromFile("src/main/resources/input")
    val lines = bufferedSource.getLines.toList
    val numbers = lines.head.split(",").map(_.toInt)
    bufferedSource.close()

    val MAX = 10
    val fishes: Array[Long] = Array.tabulate(MAX)( (x) => 0 )
    for(i <- 0 to 8){
      fishes(i) = numbers.count(number => number == i)
    }

    val NUMBER_OF_DAYS = 255

    for(i <- 0 to NUMBER_OF_DAYS){
      val aux = fishes(0)
      fishes(0) = fishes(1)
      fishes(1) = fishes(2)
      fishes(2) = fishes(3)
      fishes(3) = fishes(4)
      fishes(4) = fishes(5)
      fishes(5) = fishes(6)
      fishes(6) = fishes(7) + aux
      fishes(7) = fishes(8)
      fishes(8) = aux
    }

    var total = 0 :Long
    for(i <- 0 to 8){
      total += fishes(i)
      println(fishes(i))
    }

    println(total) // 395627 for 80 days
    // 1767323539209 for 256 days

  }
}