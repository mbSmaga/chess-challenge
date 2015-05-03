import scala.annotation.tailrec
import scala.collection.immutable.ListSet

/**
 * Created by mbsmaga on 28.04.15.
 */
object Application extends App{

  val columns = 7
  val rows = 7

  //holds every possible pair
  val allLocations: IndexedSeq[(Int, Int)] = for (i <- 0 until 7; j <- 0 until 7) yield (i, j)

  def validLocations(placedPieces: ListSet[Piece]) = {
    def filteredVector(vec: IndexedSeq[(Int, Int)], piece: Piece): IndexedSeq[(Int, Int)] = {
      val curPos = piece.currentPosition
      piece.name match {
        case 'Q' =>
          vec.filter(pair => (pair._1 != curPos._1)
            && (pair._2 != curPos._2)
            && ((pair._2 - curPos._2).abs != (pair._1 - curPos._1).abs))
        case 'B' =>
          vec.filter(pair => (pair._2 - curPos._2).abs != (pair._1 - curPos._1).abs)
        case 'K' =>
          vec.filter(pair => ((curPos._1 - pair._1).abs > 1)
            || ((curPos._2 - pair._2).abs > 1))
        case 'Z' =>
          vec.filter(pair => ((pair._2 - curPos._2).abs == 2 && (pair._1 - curPos._1).abs == 1)
            || ((pair._2 - curPos._2).abs == 1 && math.abs(pair._1 - curPos._1).abs == 2))
        case 'R' =>
          vec.filter(pair => (pair._1 == curPos._1)
            || (pair._2 == curPos._2))
      }
    }
    @tailrec
    def loop(vec: IndexedSeq[(Int, Int)], placedPieces: ListSet[Piece]): IndexedSeq[(Int, Int)] = {
      if (placedPieces.isEmpty) vec
      else
        loop(filteredVector(vec, placedPieces.head), placedPieces.tail)
    }
    if (placedPieces.isEmpty) allLocations
    else {
      val validLocations = loop(allLocations, placedPieces)
      validLocations
    }
  }

  def cyclePieces(pieces: List[Char]): Set[ListSet[Piece]] = {
    if (pieces.isEmpty) {
      Set(ListSet())
    }
    else {
      for {
        placedPieces <- cyclePieces(pieces.tail)
        // gives only valid pairs
        (x, y) <- validLocations(placedPieces)
        piece = PieceFactory(pieces.head, (x, y))
        if isValidPosition(piece, placedPieces)
      } yield (placedPieces + piece)
    }
  }

  def isValidPosition(testedPiece: Piece, pieces: ListSet[Piece]) = {
    pieces forall {
      piece => !(testedPiece.isLocationSafe(piece.currentPosition))
    }
  }

  val result = cyclePieces(List('N', 'K', 'K', 'B', 'B', 'Q', 'Q'))

  println(result.size)
}
