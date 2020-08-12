import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn

class Player(var color: Int) {
  // -1 = white, 1 = black
  require(color == -1 || color == 1)

  private val initialRowPos = if (color == -1) 0 else 7

  // Number of fig -> Name, Position
  val figures: mutable.Map[Int, (String, (Int, Int))] = mutable.Map(
    1 -> ("King", (initialRowPos, 3)),
    2 -> ("Queen", (initialRowPos, 4)),
    3 -> ("Rook", (initialRowPos, 0)),
    4 -> ("Rook", (initialRowPos, 7)),
    5 -> ("Knight", (initialRowPos, 1)),
    6 -> ("Knight", (initialRowPos, 6)),
    7 -> ("Bishop", (initialRowPos, 2)),
    8 -> ("Bishop", (initialRowPos, 5))
  ) ++ (for (i <- 0 to 7) yield (i + 9, ("Pawn", (initialRowPos + color, i)))).toMap

  val availableMoves: mutable.Map[Int, List[(Int, Int)]] = mutable.Map()

  def getAvailableMoves(figName: Int, pos: (Int, Int), board: Array[Array[Int]]): ListBuffer[(Int, Int)] = {

    // Check fig positions for several cases:
    //
    // First: current positions is occupied by player's side, next by other side or empty
    // Second: current position is empty and next is occupied by other side or empty
    //
    // If so, player can move forward, otherwise nex position is obstacle
    val noObstacle = (currentPos: (Int, Int), nextPos: (Int, Int)) => {
      val currentFig = board(currentPos._1)(currentPos._2)
      val nextFig = board(nextPos._1)(nextPos._2)
      (currentFig == 0 || Math.signum(currentFig) == color) && (nextFig == 0 || Math.signum(nextFig) == -color)

    }

    def fKing(pos: (Int, Int)) = {
      val positions = new ListBuffer[(Int, Int)]()
      val rowBounds = Array(pos._1 - 1, pos._1 + 1)
      rowBounds.foreach(row => {
        if (board(2)(row) == 0 || Math.signum(board(2)(row)) == -color) positions += Tuple2(2, row)
        for (col <- pos._2 - 1 to pos._2 + 1) if (board(row)(col) == 0 || Math.signum(board(row)(col)) == -color)
          positions += Tuple2(row, col)
      })
      positions
    }

    def fRook(pos: (Int, Int)) = {
      val positions = new ListBuffer[(Int, Int)]()

      // Scan vertically to detect obstacles
      def scanOnRows() = {
        // Need to check both sides of current pos:
        // towards 0 and towards 7
        Array(0, 7).foreach(endPoint => {
          val order = if (endPoint == 0) -1 else 1  // Use this val to be able iterate in ascending and descending orders
          var row = pos._1 // start position

          // Iterating over constant column
          // First check if we are still on board and then if no obstacles in the next position
          while ((row * order < endPoint) && noObstacle((row, pos._2), (row + order, pos._2)) ) {
            row += order
            positions += Tuple2(row, pos._2)
          }})
      }

      // Scan horizontally
      def scanOnCols() = {
        // Doing the same as scanOnRows but row is constant
        Array(0, 7).foreach(endPoint => {
          val order = if (endPoint == 0) -1 else 1
          var col = pos._2

          while ((col * order < endPoint) && noObstacle((pos._1, col), (pos._1, col + order))) {
            col += order
            positions += Tuple2(pos._1, col)
          }})
      }

      scanOnRows()
      scanOnCols()
      positions
    }

    def fBishop(pos: (Int, Int)) = {
      val positions = new ListBuffer[(Int, Int)]()

      def scanDiag1() = {
        Array(0, 7).foreach(endPoint => {
          val order = if (endPoint == 0) -1 else 1
          var row = pos._1
          var col = pos._2

          // First check if we are still on board and then if no obstacles in the next position
          while ((row * order < endPoint) && (col * order < endPoint) && noObstacle((row, col), (row + order, col + order))) {
            row += order
            col += order
            positions += Tuple2(row, col)
          }})
      }

      def scanDiag2() = {
        Array((0, 7), (7, 0)).foreach(endPoints =>{
          val order = if (endPoints._1 == 0) -1 else 1
          var row = pos._1
          var col = pos._2

          while ((row*order < endPoints._1) && (col*(-1)*order < endPoints._2) && noObstacle((row, col), (row + order, col - order))) {
            row += order
            col -= order
            positions += Tuple2(row, col)
          }})
      }

      scanDiag1()
      scanDiag2()
      positions
    }

    def fQueen(pos: (Int, Int)) = {
      // Queen moves just like Rook and Bishop together
      val diagonalPositions = fBishop(pos)
      val linesPositions = fRook(pos)

      diagonalPositions :++ linesPositions
    }

    def fKnight(pos: (Int, Int)) = {
      val positions = new ListBuffer[(Int, Int)]()
      Array((1,1), (-1,-1), (1, -1), (-1,1)).foreach(signs => {
        positions += Tuple2(pos._1 + 2*signs._1, pos._2 + 1*signs._2)
        positions += Tuple2(pos._1 + 1*signs._1, pos._2 + 2*signs._2)
      })

      val checkIfOnBoard = (pos: (Int, Int)) => (0 to 7).contains(pos._1) && (0 to 7).contains(pos._2)
      val checkIfObstacle = (pos: (Int, Int)) => Math.signum(board(pos._1)(pos._2)) != color

      positions.filter(pos => checkIfOnBoard(pos) && checkIfObstacle(pos))
    }

    def fPawn(pos: (Int, Int)) = {
      val movePositions = new ListBuffer[(Int, Int)]()
      val nextRow = pos._1 + color

      val checkIfOnBoard = (pos: (Int, Int)) => (0 to 7).contains(pos._1) && (0 to 7).contains(pos._2)
      val checkIfEnemy = (pos: (Int, Int)) => Math.signum(board(pos._1)(pos._2)) == -color

      // If haven't done any move is possible to move in 2 positions forward
      if (pos._1 == initialRowPos + color) {
        for (row <- Array(nextRow, nextRow + color)) {
          if (board(row)(pos._2) == 0) movePositions += Tuple2(row, pos._2)
        }
      }
      else if ((0 to 7).contains(nextRow) && board(nextRow)(pos._2) == 0) movePositions += Tuple2(nextRow, pos._2)

      val attackPositions = ListBuffer((nextRow, pos._2 - 1), (nextRow, pos._2 + 1)).filter(move =>
        checkIfOnBoard(move) && checkIfEnemy(move))

      movePositions :++ attackPositions
    }



  }
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
  def initBoard(players: Array[Player]): Array[Array[Int]] = {
    val board = Array.ofDim[Int](8, 8)

    for (player <- players) {
      player.figures.foreach(pair => {
        val pos = pair._2._2
        val numFig = pair._1
        board(pos._1)(pos._2) = numFig
        })
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
    val chosenFigName = player.figures(chosenFigNum)._1


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