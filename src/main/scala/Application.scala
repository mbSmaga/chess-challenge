import scala.io.StdIn

/**
 * Created by mbsmaga on 28.04.15.
 */
object Application extends App {
  println("\n### Chess task ###\n")
  print("Insert column size: ")
  val columns = getIntUserInput.getOrElse(System.exit(-1))
  print("Insert row size: ")
  val rows = getIntUserInput.getOrElse(System.exit(-1))
  print("Insert all the pieces " +
    "(Q - queen, K - King, B - Bishop, N - Knight, R - Rook)\n" +
    "(eg. if you want to place two Rooks and one Queen, the input should look like this: RRQ)\n" +
    "Pieces: ")
  val userList = StdIn.readLine().toList
  println("\nProcessing...")

  val chessBoard = new ChessBoard(columns.asInstanceOf[Int], rows.asInstanceOf[Int])
  val preparedPiecesList = chessBoard.preparePiecesList(userList)
  val timeBefore = System.currentTimeMillis()
  val resultsList = chessBoard.start(preparedPiecesList)
  val timeAfter = System.currentTimeMillis()

  println("Number of results: " + resultsList.size)
  println("Computation time: " + (timeAfter - timeBefore) / 1000 + " s")

  /**
   * Gets user numeric input with validation.
   * @return
   */
  def getIntUserInput(): Option[Int] = {
    try {
      Some(StdIn.readInt())
    } catch {
      case nfe: NumberFormatException => {
        print("Please input valid number!: ")
        getIntUserInput()
      }
      case e: Exception => {
        print("Unsupported exception! Shutting down.")
        e.printStackTrace()
        None
      }
    }
  }
}
