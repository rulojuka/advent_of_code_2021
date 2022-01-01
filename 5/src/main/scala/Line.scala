case class Line(var x1: Int, var y1: Int, var x2: Int, var y2: Int) {

  if(isHorizontal && x1 > x2){
    val aux = x1
    x1 = x2
    x2 = aux
  }

  if(isVertical && y1 > y2){
    val aux = y1
    y1 = y2
    y2 = aux
  }

  if(isDiagonal && x1 > x2){
    val auxX = x1
    x1 = x2
    x2 = auxX
    val auxY = y1
    y1 = y2
    y2 = auxY
  }

  def isVertical: Boolean = {
    x1 == x2
  }

  def isHorizontal: Boolean = {
    y1 == y2
  }

  def isDiagonalOne: Boolean = {
    x2-x1 == y2-y1
  }

  def isDiagonalTwo: Boolean = {
    x2-x1 == (-1)* (y2-y1)
  }

  private def isDiagonal: Boolean = {
    isDiagonalOne || isDiagonalTwo
  }

}

/*

Origin
o-------------> x axis (starts at 0)
|
|
|
|
|
|
V
y axis (starts at 0)

 */
