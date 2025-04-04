object AtariGo {
  type Board = List[List[Stone.Value]] // Usar Stone.Value em vez de Stone
  type Coord2D = (Int, Int)

  object Stone extends Enumeration {
    type Stone = Value
    val Black, White, Empty = Value
  }

  case class MyRandom(seed: Long) {
    def nextInt(n: Int): (Int, MyRandom) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRand = MyRandom(newSeed)
      val number = ((newSeed >>> 16).toInt.abs) % n  // Correção aqui
      (number, nextRand)
    }
  }


  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val size = lstOpenCoords.size
    val (index, newRand) = rand.nextInt(size)
    val selectedCoord = lstOpenCoords(index)
    (selectedCoord, newRand)
  }

  def play(board: Board, player: Stone.Value, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    if (!lstOpenCoords.contains(coord)) {
      (None, lstOpenCoords)
    } else {
      val (row, col) = coord
      val newRow = board(row).updated(col, player)
      val newBoard = board.updated(row, newRow)
      val newLstOpenCoords = lstOpenCoords.filterNot(_ == coord)
      (Some(newBoard), newLstOpenCoords)
    }
  }

  def playRandomly(board: Board, r: MyRandom, player: Stone.Value, lstOpenCoords: List[Coord2D],
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Board, MyRandom, List[Coord2D]) = {
    val (coord, newRand) = f(lstOpenCoords, r)
    val (maybeNewBoard, newLstOpenCoords) = play(board, player, coord, lstOpenCoords)
    maybeNewBoard match {
      case Some(newBoard) => (newBoard, newRand, newLstOpenCoords)
      case None => (board, r, lstOpenCoords)
    }
  }

  def printBoard(board: Board): Unit = {
    println("  " + (0 until board.size).mkString(" "))
    for ((row, i) <- board.zipWithIndex) {
      print(i + " ")
      for (stone <- row) {
        val symbol = stone match {
          case Stone.Black => "B"
          case Stone.White => "W"
          case Stone.Empty => "."
        }
        print(symbol + " ")
      }
      println()
    }  
  }

  def main(args: Array[String]): Unit = {
    val size = 7
    var board: Board = List.fill(size)(List.fill(size)(Stone.Empty))
    var lstOpenCoords: List[Coord2D] = (for {
      row <- 0 until size
      col <- 0 until size
    } yield (row, col)).toList

    var rand = MyRandom(System.currentTimeMillis())
    var player = Stone.Black

    while (lstOpenCoords.nonEmpty) {
      printBoard(board)
      println(s"Player $player, enter row and column (e.g., 3 4):")
      val input = scala.io.StdIn.readLine()
      val coords = input.split(" ").map(_.toInt)
      if (coords.length == 2) {
        val coord = (coords(0), coords(1))
        val (maybeNewBoard, newLstOpenCoords) = play(board, player, coord, lstOpenCoords)
        maybeNewBoard match {
          case Some(newBoard) =>
            board = newBoard
            lstOpenCoords = newLstOpenCoords
            player = if (player == Stone.Black) Stone.White else Stone.Black
          case None => println("Invalid move! Try again.")
        }
      } else {
        println("Invalid input! Please enter two numbers separated by space.")
      }
    }

    println("Game over!")
    printBoard(board)
  }
}