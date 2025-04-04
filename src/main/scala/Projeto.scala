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
    val size = 3
    val initialBoard: Board = List.fill(size)(List.fill(size)(Stone.Empty))
    val initialLstOpenCoords: List[Coord2D] = (for {
      row <- 0 until size
      col <- 0 until size
    } yield (row, col)).toList

    val rand = MyRandom(42)

    // Teste 1: Jogador Preto joga na posição (1,1)
    val (board1, openCoords1) = play(initialBoard, Stone.Black, (1,1), initialLstOpenCoords)
    println("Teste 1: Jogador Preto joga na posição (1,1)")
    printBoard(board1.getOrElse(initialBoard))
    println()

    // Teste 2: Jogador Branco joga na posição (0,0)
    val (board2, openCoords2) = play(board1.getOrElse(initialBoard), Stone.White, (0,0), openCoords1)
    println("Teste 2: Jogador Branco joga na posição (0,0)")
    printBoard(board2.getOrElse(initialBoard))
    println()

    // Teste 3: Jogador Preto tenta jogar novamente em (1,1) (inválido)
    val (board3, openCoords3) = play(board2.getOrElse(initialBoard), Stone.Black, (1, 1), openCoords2)
    println("Teste 3: Jogador Preto tenta jogar novamente em (1,1) (inválido)")
    printBoard(board3.getOrElse(board2.get)) // Mantém o último estado válido
    println("(Esperado: mesma saída do Teste 2)")
    println()


    // Teste 4: Jogada aleatória
    val (randomBoard, _, _) = playRandomly(board2.getOrElse(initialBoard), rand, Stone.Black, openCoords2, randomMove)
    println("Teste 4: Jogador Preto faz uma jogada aleatória")
    printBoard(randomBoard)
  }
}