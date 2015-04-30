/**
 * Created by mbsmaga on 28.04.15.
 */
object Application extends App{

  val columns = 7
  val rows = 7

  def cyclePieces(pieces: List[Char]): Set[Vector[Piece]] = {
    if (pieces.isEmpty) {
      Set(Vector())
    }
    else {
      for {
        placedPieces <- cyclePieces(pieces.tail)
        x <- 0 until columns
        y <- 0 until rows
        piece = PieceFactory(pieces.head, (x, y))
        if isValidPosition(piece, placedPieces)
      } yield (piece +: placedPieces).sortBy(p => (p.currentPosition._1, p.currentPosition._2))
    }
  }

  def isValidPosition(curPiece: Piece, pieces: Vector[Piece]) = {
    pieces forall {
      piece => !(piece.isLocationSafe(curPiece.currentPosition)) && !(curPiece.isLocationSafe(piece.currentPosition))
    }
  }

  val result = cyclePieces(List('Z', 'K', 'K', 'B', 'B', 'Q', 'Q'))

  println(result.size)
}
