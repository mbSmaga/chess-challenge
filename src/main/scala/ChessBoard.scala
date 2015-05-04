import scala.annotation.tailrec
import scala.collection.immutable.ListSet
import scala.io.StdIn

/**
 * Created by mbsmaga on 04.05.15.
 */
class ChessBoard(val columns: Int, val rows: Int) {
  //order, in which pieces will be placed on the board
  private val orderOfPieces: List[Char] = List('Q', 'B', 'R', 'K', 'N')
  //List of every possible pair (colNo, rowNo)
  private val allLocations: List[(Int, Int)] = (for (i <- 0 until columns; j <- 0 until rows) yield (i, j)).toList

  /**
   * Gets the list from the user input and returns it in an order given by orderOfPieces
   * @param piecesToBePlaced
   * @return
   */
  def preparePiecesList(piecesToBePlaced: List[Char]): List[Char] = {
    @tailrec
    def loop(listOrdered: List[Char], accuList: List[Char]): List[Char] = {
      if (listOrdered.isEmpty) accuList
      else {
        loop(listOrdered.tail, piecesToBePlaced.filter(char => (char == listOrdered.head)) ::: accuList)
      }
    }
    loop(orderOfPieces, List[Char]())
  }

  /**
   * Gets the ordered pieces from the user input and returns all possible variations
   * of placing them on the board in the way, that no peace can attack another directly
   * @param pieces
   * @return
   */
  def start(pieces: List[Char]): Set[ListSet[Piece]] = {
    cyclePieces(pieces: List[Char])
  }

  /**
   * Takes list of currently placed pieces on the board and returns all locations,
   * which are safe to place the next piece.
   * Still, you need to check if newly placed pieces are not attaking any of
   * the already placed ones. (This is done by isValidPosition() function)
   * @param placedPieces
   * @return
   */
  private def validLocations(placedPieces: ListSet[Piece]) = {
    def filterLocations(locations: List[(Int, Int)], piece: Piece): List[(Int, Int)] = {
      val curPos = piece.currentPosition
      piece.name match {
        case 'Q' =>
          locations.filterNot(position => (position._1 == curPos._1)
            || (position._2 == curPos._2)
            || ((position._2 - curPos._2).abs == (position._1 - curPos._1).abs))
        case 'B' =>
          locations.filter(position => (position._2 - curPos._2).abs != (position._1 - curPos._1).abs)
        case 'K' =>
          locations.filter(position => ((curPos._1 - position._1).abs > 1)
            || ((curPos._2 - position._2).abs > 1))
        case 'N' =>
          locations.filterNot(position => ((position._2 - curPos._2).abs == 2 && (position._1 - curPos._1).abs == 1)
            || ((position._2 - curPos._2).abs == 1 && math.abs(position._1 - curPos._1).abs == 2))
        case 'R' =>
          locations.filterNot(position => (position._1 == curPos._1)
            || (position._2 == curPos._2))
        case _ => locations
      }
    }
    /**
     * helper function to iterate over already placed pieces
     * @param locations
     * @param placedPieces
     * @return
     */
    @tailrec
    def loop(locations: List[(Int, Int)], placedPieces: ListSet[Piece]): List[(Int, Int)] = {
      if (placedPieces.isEmpty) locations
      else
        loop(filterLocations(locations, placedPieces.head), placedPieces.tail)
    }
    if (placedPieces.isEmpty) allLocations
    else {
      loop(allLocations, placedPieces)
    }
  }

  /**
   * Cycles over the pieces and puts them on the board, if they are valid.
   * Uses backtracking recursion with depth first search
   * @param pieces
   * @return
   */
  private def cyclePieces(pieces: List[Char]): Set[ListSet[Piece]] = {
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

  /**
   * Checks if the testedPiece can be placed on it's location
   * @param testedPiece
   * @param pieces
   * @return
   */
  private def isValidPosition(testedPiece: Piece, pieces: ListSet[Piece]) = {
    pieces forall {
      piece => !(testedPiece.isLocationSafe(piece.currentPosition))
    }
  }
}
