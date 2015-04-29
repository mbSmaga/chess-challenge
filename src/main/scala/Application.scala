/**
 * Created by mbsmaga on 28.04.15.
 */
object Application extends App{
  def queens(boardSize: Int): Set[List[(Int, Int)]] = {
    def placeQueens(currentQueen: Int): Set[List[(Int, Int)]] = {
      if (currentQueen == 0) Set(List())
      else
        for {
          queens <- placeQueens(currentQueen - 1)
          column <- 0 until boardSize
          row <- 0 until boardSize
          if (isSafe((row, column), queens))
        } yield ((row, column) :: queens).sortBy(r => (r._1, r._2))
    }
    placeQueens(boardSize)
  }
  def isSafe(pair: (Int, Int), queens: List[(Int, Int)]): Boolean = {
    queens forall {
      case (r, c) => pair._2 != c && pair._1 != r && math.abs(pair._2 - c) != math.abs(pair._1 - r)
    }
  }
  def show(queens: List[Int]) = {
    //TODO: Implement this method
  }
  println(queens(8).size)
}
