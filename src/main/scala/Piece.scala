/**
 * Created by mbsmaga on 30.04.15.
 */
abstract class Piece {
  val currentPosition: (Int, Int)
  val name: Char

//  override def toString = name.toString

  def isLocationSafe(testedPosition: (Int, Int)) = {

    val deltaX = (testedPosition._1 - currentPosition._1).abs
    val deltaY = (testedPosition._2 - currentPosition._2).abs

    testedPosition == currentPosition || checkLocation(deltaX, deltaY)
  }

  def checkLocation (position: (Int, Int)): Boolean
}

case class Queen(override val currentPosition: (Int, Int), name: Char) extends Piece {
  override def checkLocation(step: (Int, Int)) =
    step._1 == 0 || step._2 == 0 || step._1 == step._2
}

case class King(override val currentPosition: (Int, Int), name: Char) extends Piece {
  override def checkLocation(step: (Int, Int)) =
    step._1 <= 1 && step._2 <= 1
}

case class Bishop(override val currentPosition: (Int, Int), name: Char) extends Piece {
  override def checkLocation(step: (Int, Int)) =
    step._1 == step._2
}

case class Rook(override val currentPosition: (Int, Int), name: Char) extends Piece {
  override def checkLocation(step: (Int, Int)) =
    step._1 == 0 || step._2 == 0
}

case class Knight(override val currentPosition: (Int, Int), name: Char) extends Piece {
  override def checkLocation(step: (Int, Int)) =
    (step._1 == 1 && step._2 == 2) || (step._1 == 2 && step._2 == 1)
}
object PieceFactory {
  def apply(name: Char, position: (Int, Int)) = {
    name match {
      case 'Q'   => new Queen(position, name)
      case 'K'   => new King(position, name)
      case 'B'   => new Bishop(position, name)
      case 'R'   => new Rook(position, name)
      case 'N'   => new Knight(position, name)
    }
  }
}
