/**
 * Created by mbsmaga on 30.04.15.
 */
abstract case class Piece(name: Char, currentPosition: (Int, Int)) {

  override def toString = name.toString

  def isLocationSafe(testedPosition: (Int, Int)) = {

    val deltaX = (testedPosition._1 - currentPosition._1).abs
    val deltaY = (testedPosition._2 - currentPosition._2).abs

    testedPosition == currentPosition || checkLocation(deltaX, deltaY)
  }

  def checkLocation (position: (Int, Int)): Boolean
}

class Queen(currentPosition: (Int, Int)) extends Piece('Q', currentPosition) {
  override def checkLocation(step: (Int, Int)) =
    step._1 == 0 || step._2 == 0 || step._1 == step._2
}

class King(currentPosition: (Int, Int)) extends Piece('K', currentPosition) {
  override def checkLocation(step: (Int, Int)) =
    step._1 <= 1 && step._2 <= 1
}

class Bishop(currentPosition: (Int, Int)) extends Piece('B', currentPosition) {
  override def checkLocation(step: (Int, Int)) =
    step._1 == step._2
}

class Rook(currentPosition: (Int, Int)) extends Piece('R', currentPosition) {
  override def checkLocation(step: (Int, Int)) =
    step._1 == 0 || step._2 == 0
}

class Zbych(currentPosition: (Int, Int)) extends Piece('N', currentPosition) {
  override def checkLocation(step: (Int, Int)) =
    (step._1 == 1 && step._2 == 2) || (step._1 == 2 && step._2 == 1)
}
object PieceFactory {
  def apply(name: Char, position: (Int, Int)) = {
    name match {
      case 'Q'   => new Queen(position)
      case 'K'    => new King(position)
      case 'B'  => new Bishop(position)
      case 'R'    => new Rook(position)
      case 'Z'   => new Zbych(position)
    }
  }
}
