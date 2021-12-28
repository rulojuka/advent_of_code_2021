import scala.io.Source

object Program {

  def arraySum(array1: Array[Int], array2: Array[Int]): Array[Int] = {
    array1.zipWithIndex.map(tuple => tuple._1 + array2(tuple._2))
  }

  def calculateGammaRate(array: Array[Int], totalLines: Int): Int = {
    val binaryArray = array.map(element => {
      if(element > totalLines/2) 1
      else 0
    })
    calculateDecimalFromBinaryArray(binaryArray)
  }

  def calculateDecimalFromBinaryArray(binaryArray: Array[Int]): Int = {
    var multiplier = 1
    var result = 0
    binaryArray.reverse.foreach{ bit =>
      if(bit==1) result += multiplier
      multiplier *= 2
    }
    result
  }

  def calculateArrayUsingFunction(arrays: List[Array[Int]],
                                  numberOfBits: Int,
                                  arrayRemovalFunction: (Array[Int], Int, Int) => Int
                                 ): Array[Int] = {
    var bitIndex = 0
    var arraysLeft = arrays
    while(arraysLeft.size > 1 && bitIndex < numberOfBits){
      val sumOfArrays = arraysLeft.reduce(arraySum)
      val currentBit = arrayRemovalFunction(sumOfArrays, bitIndex, arraysLeft.size)
      arraysLeft = arraysLeft.filter( array => array(bitIndex) == currentBit )
      bitIndex = bitIndex+1
    }
    arraysLeft(0)
  }

  def oxygenGeneratorFunction(array: Array[Int], index: Int, total: Int):Int = {
    if(array(index) > (total-1)/2) 1
    else 0
  }

  def cO2ScrubberFunction(array: Array[Int], index: Int, total: Int):Int = {
    if(oxygenGeneratorFunction(array,index,total) == 0) 1
    else 0
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/resources/input.txt").getLines().toList
    val arrays = lines.map( _.toArray.map( _.toInt - '0') )
    val totalLines = arrays.size
    val sumOfArrays = arrays.reduce(arraySum)
    val numberOfBits = sumOfArrays.size
    val onlyValidBits = (1 << numberOfBits) - 1

    val gammaRate = calculateGammaRate(sumOfArrays,totalLines)
    println(gammaRate)

    val epsilonRate = ~gammaRate & onlyValidBits
    println(epsilonRate)

    println(gammaRate * epsilonRate) // 4103154

    val oxygenGeneratorRatingArray = calculateArrayUsingFunction(arrays,numberOfBits,oxygenGeneratorFunction)
    val oxygenGeneratorRating = calculateDecimalFromBinaryArray(oxygenGeneratorRatingArray)
    println(oxygenGeneratorRating)

    val CO2ScrubberRatingArray = calculateArrayUsingFunction(arrays,numberOfBits,cO2ScrubberFunction)
    val cO2ScrubberRating = calculateDecimalFromBinaryArray(CO2ScrubberRatingArray)
    println(cO2ScrubberRating)

    println(oxygenGeneratorRating * cO2ScrubberRating) // 4245351
  }
}
