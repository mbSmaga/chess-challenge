/**
 * Created by mbsmaga on 28.04.15.
 */
object Application extends App{

  val columns = 7
  val rows = 7

  //holds every possible pair
  val vector: IndexedSeq[(Int, Int)] = for (i <- 0 until 7; j <- 0 until 7) yield (i, j)

  def vectorOfPairs(configuration: Vector[Piece]) = {
    def filteredVector(vec: IndexedSeq[(Int, Int)], piece: Piece): IndexedSeq[(Int, Int)] = {
      val curPos = piece.currentPosition
      piece.toString match {
        case "Q" =>
          vec.filter(pair => (pair._1 != curPos._1)
            && (pair._2 != curPos._2)
            && ((pair._2 - curPos._2).abs != (pair._1 - curPos._1).abs))
        case "B" =>
          vec.filter(pair => (pair._2 - curPos._2).abs != (pair._1 - curPos._1).abs)
        case "K" =>
          vec.filter(pair => ((curPos._1 - pair._1).abs > 1)
            || ((curPos._2 - pair._2).abs > 1))
        case "Z" =>
          vec.filter(pair => ((pair._2 - curPos._2).abs == 2 && (pair._1 - curPos._1).abs == 1)
            || ((pair._2 - curPos._2).abs == 1 && math.abs(pair._1 - curPos._1).abs == 2))
        case "R" =>
          vec.filter(pair => (pair._1 == curPos._1)
            || (pair._2 == curPos._2))
        case _ => vec
      }
    }
    def loop(vec: IndexedSeq[(Int, Int)], confi: Vector[Piece]): IndexedSeq[(Int, Int)] = {
      if (confi.isEmpty) vec
      else
        loop(filteredVector(vec, confi.head), confi.tail)
    }
    if (configuration.isEmpty) vector
    else loop(vector, configuration)
  }

  def cyclePieces(pieces: List[Char]): Set[Vector[Piece]] = {
    if (pieces.isEmpty) {
      Set(Vector())
    }
    else {
      for {
        placedPieces <- cyclePieces(pieces.tail)
        (x, y) <- vectorOfPairs(placedPieces)
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
