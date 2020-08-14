import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn

class Player(var color: Int) {
  // -1 = white, 1 = black
  require(color == -1 || color == 1)

  private val initialRowPos = if (color == 1) 0 else 7

  // Number of fig -> Name, Position
  val figures: mutable.Map[Int, (String, (Int, Int))] = mutable.Map(
    1*color -> ("King", (initialRowPos, 3)),
    2*color -> ("Queen", (initialRowPos, 4)),
    3*color -> ("Rook", (initialRowPos, 0)),
    4*color -> ("Rook", (initialRowPos, 7)),
    5*color -> ("Knight", (initialRowPos, 1)),
    6*color-> ("Knight", (initialRowPos, 6)),
    7*color -> ("Bishop", (initialRowPos, 2)),
    8*color -> ("Bishop", (initialRowPos, 5))
  ) ++ (for (i <- 0 to 7) yield ((i + 9)*color, ("Pawn", (initialRowPos + color, i)))).toMap

  var availableMoves: mutable.Map[Int, ListBuffer[(Int, Int)]] = mutable.Map[Int, ListBuffer[(Int, Int)]]()

  def updateAvailableMoves(board: Array[Array[Int]]): Unit = {

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
      val rows = (pos._1 - 1 to pos._1 + 1).toArray.filter(coordinate => (0 to 7).contains(coordinate))
      val cols = (pos._2 - 1 to pos._2 + 1).toArray.filter(coordinate => (0 to 7).contains(coordinate))

      rows.foreach(row => {
        cols.foreach(col => {
          if (board(row)(col) == 0) positions += Tuple2(row, col)
        })
      })

      positions
    }

    def fRook(pos: (Int, Int)) = {
      val positions = new ListBuffer[(Int, Int)]()

      // Scan vertically to detect obstacles
      def scanOnRows(): Unit = {
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
      def scanOnCols(): Unit = {
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

      def scanDiag1(): Unit = {
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

      def scanDiag2(): Unit = {
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

    type F = ((Int, Int)) => ListBuffer[(Int, Int)]

    val applyFunction: Map[String, F] = Map(
      "King" -> fKing,
      "Queen" -> fQueen,
      "Rook" -> fRook,
      "Knight" -> fKnight,
      "Bishop" -> fBishop,
      "Pawn" -> fPawn
    )

    for (figure <- figures) {
      val figNum = figure._1
      val figName = figure._2._1
      val figPos = figure._2._2
      availableMoves.update(figNum, applyFunction(figName)(figPos))
    }
  }
}

class GameHost {
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

  def displayBoard(board: Array[Array[Int]]) = {
    board.foreach(line => println(line.mkString(" ")))
  }

  def makeMove(player: Player, board: Array[Array[Int]]): Unit = {

    @tailrec
    def getPosition(): Array[Int] = {
      println("Please enter position(row, column) of a figure to move: ")
      val posArray = StdIn.readLine().split(" ").map(element => element.toInt)

      if ((0 to 7).contains(posArray(0)) && (0 to 7).contains(posArray(1))) posArray
      else {println("Position is invalid!"); getPosition()}
    }

    displayBoard(board)
    val figPos = getPosition()  // returns Array(row, col)
    val figNum = board(figPos(0))(figPos(1))

    // check if chosen figure is your color
    if (Math.signum(figNum) == player.color) {
      val moves = player.availableMoves(figNum)

      // print all available moves for figure with index to each
      println("Available moves:")
      moves.foreach(move => println(moves.indexOf(move).toString + " " + move.toString()))

      println("Select move: ")
      val selectedMoveIndex = StdIn.readLine().toInt

      // if input is -1 choose figure again
      if (selectedMoveIndex == -1) makeMove(player, board)
      else {
        val movePositions = moves(selectedMoveIndex)
        board(movePositions._1)(movePositions._2) = figNum
        board(figPos(0))(figPos(1)) = 0
      }
    }
    else {println("Selected figure is not your color"); makeMove(player, board)}
  }

  def checkEnd(players: Map[Int, Player]): Int = {
    // return a color of a winner if checkmate or 0 otherwise

    def isCheck(kingPos: (Int, Int), attacker: Player) = {
      val attackerPossibleMoves = attacker.availableMoves
      if (attackerPossibleMoves.values.exists(list => list.contains(kingPos))) true
      else false
    }

    def isCheckmate(defender: Player, attacker: Player) = {
      val allPossibleKingMoves = defender.availableMoves(defender.color)
      allPossibleKingMoves.forall(pos => isCheck(pos, attacker))
    }

    if (isCheckmate(players(1), players(-1))) -1
    else if (isCheckmate(players(-1), players(1))) 1
    else 0
  }

  def initGame(): (Player, Player, Array[Array[Int]]) = {
    val playerBlack = new Player(1)
    val playerWhite = new Player(-1)
    val board = initBoard(Array(playerWhite, playerBlack))

    (playerBlack, playerWhite, board)
  }
}

object Game extends App {
  // TODO Test this part, refactor it, and add other features
  val gameHost = new GameHost()
  val (playerBlack, playerWhite, board) = gameHost.initGame()
  val players = Map(1 -> playerWhite, -1 -> playerBlack)

  var isEnd = 0
  var currentPlayer = 1  // first turn is white's turn

  while (isEnd == 0) {
    gameHost.makeMove(players(currentPlayer), board)
    isEnd = gameHost.checkEnd(players)
    currentPlayer *= -1
  }
}