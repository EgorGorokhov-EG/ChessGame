import scala.collection.mutable
import scala.io.StdIn

class Player(var color: Int) {
  // 1 = white, -1 = black
  require(color == -1 || color == 1)

  // Map with the names of figures(Value) and their numbers(Key)
  val figures: mutable.Map[Int, String] = mutable.Map(
    1 ->"King",
    2 -> "Queen",
    3 -> "Rook",
    4 -> "Rook",
    5 -> "Knight",
    6 -> "Knight",
    7 -> "Bishop",
    8 -> "Bishop"
  ) ++ (for (n <- 9 to 16) yield (n, "Pawn")).toMap

  val availableMoves: mutable.Map[Int, Array[(Int, Int)]] = mutable.Map()

}

class Game {
  // functions to check correctness of selected move for certain figure
  val fKing = (pos: (Int, Int), move: (Int, Int)) => (move._1 - pos._1).abs <= 1 && (move._2 - pos._2).abs <= 1

  val fQueen = (pos: (Int, Int), move: (Int, Int)) =>
    (pos._1 == move._1 || pos._2 == move._2) || ((pos._1 - move._1).abs == (pos._2 - move._2).abs)

  val fRook = (pos: (Int, Int), move: (Int, Int)) => pos._1 == move._1 || pos._2 == move._2

  val fKnight = (pos: (Int, Int), move: (Int, Int)) =>
    ((move._1 - pos._1).abs == 2 && (move._2 - pos._2) == 1) || ((move._2 - pos._2).abs == 2 && (move._1 - pos._1) == 1)

  val fBishop = (pos: (Int, Int), move: (Int, Int)) => (pos._1 - move._1).abs == (pos._2 - move._2).abs

  def fPawn(pos: (Int, Int), move: (Int, Int), color: Int): Boolean = {
    if (color == -1) move._2 - pos._2 == 1 && (move._1 - pos._1).abs == 1
    else move._2 - pos._2 == -1 && (move._1 - pos._1).abs == 1
  }

  type F = ((Int, Int), (Int, Int)) => Boolean

  // Map containing functions to check if a chosen position is correct for selected figure
  val checkMoves: Map[String, F] = Map(
    "King" -> fKing,
    "Queen" -> fQueen,
    "Rook" -> fRook,
    "Knight" -> fKnight,
    "Bishop" -> fBishop
  )

  // Board represented as a 2d array, white figs down, black up.
  // 1 = white, -1 = black
  def initBoard(): Array[Array[Int]] = {
    val board = Array.ofDim[Int](8, 8)

    // Pawns initialization
    for (i <- 1 to board.length) {
      board(1)(i) = -(i + 8)
      board(6)(i) = i + 8
    }

    for (i <- Array(0, 7)) {
      val side = if (i == 0) -1 else 1
      //King
      board(i)(3) = 1*side

      //Queen
      board(i)(4) = 2*side

      //Rooks
      board(i)(0) = 3*side
      board(i)(7) = 4*side

      //Knights
      board(i)(1) = 5*side
      board(i)(6) = 6*side

      //Bishops
      board(i)(2) = 6*side
      board(i)(5) = 7*side
    }
    board
  }

  def makeMove(player: Player, board: Array[Array[Int]]): (Int, Int) = {
    // TODO Add possibility for player to select move from available moves for chosen figure

    def getInputFromPlayer: ((Int, Int), (Int, Int)) = {
      println("Enter coordinates(row, column) of figure to move: ")
      val posArray = StdIn.readLine().split(" ")
      val pos = (posArray(1).toInt, posArray(0).toInt)

      // Change order because in checkMoves passes x,y coordinates
      // but board in rows,cols coordinates
      // (may change it later)

      println("Enter coordinates(row, column) of move: ")
      val moveArray = StdIn.readLine().split(" ")
      val move = (moveArray(1).toInt, moveArray(0).toInt)

      (pos, move)
    }

    val (pos, move) = getInputFromPlayer
    val chosenFigNum = board(pos._1)(pos._2)
    val chosenFigName = player.figures(chosenFigNum)


    def checkIfMoveValid: Boolean = {
      // Function checks if move is valid for the fig
      // and no other figures on the way

      val moveValid = if (chosenFigName == "Pawn") fPawn(pos, move, player.color) else checkMoves(chosenFigName)(pos, move)

      // checks if player selected figure of his color
      val rightSide = (player.color == -1 && chosenFigNum < 0) || (player.color == 1 && chosenFigNum > 0)

      var noObstacles = true

      // for Knight there are always no obstacles
      if (chosenFigName != "Knight") {
        (pos._1 + 1 until move._1).foreach(
          col => (pos._2 + 1 until move._2).foreach(
            row => {if (board(row)(col) != 0) noObstacles = false}))}

      // adds condition if move position is not occupied by figure of the same color
      noObstacles = noObstacles && (board(move._2)(move._1) != player.color)

      moveValid && noObstacles && rightSide
    }

    def checkIfEnemyFigDefeated: Boolean = board(move._2)(move._1) == player.color * -1

    if (checkIfMoveValid) {
      // check if enemy fig defeated, move fig to new position, remove it from the previous one
      if (checkIfEnemyFigDefeated) player.figures -= chosenFigNum
      board(move._2)(move._1) = chosenFigNum
      board(pos._2)(pos._1) = 0
      move
    }
    else {
      println("Chosen move is invalid, please make another one.")
      makeMove(player, board)
    }
  }

  def checkEnd(player: Player, board: Array[Array[Int]], lastMovedPos: (Int, Int)) = {
    def isCheck(pos: (Int, Int)): Boolean = {

    }

  }
}