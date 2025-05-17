object Game {

  type Board = List[List[Stone.Value]]
  type Coord2D = (Int, Int)

  object Stone extends Enumeration {
    type Stone = Value
    val Black, White, Empty = Value
  }

  case class MyRandom(seed: Long) {
    def nextInt(n: Int): (Int, MyRandom) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRand = MyRandom(newSeed)
      val number = ((newSeed >>> 16).toInt.abs) % n
      (number, nextRand)
    }
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val size = lstOpenCoords.size
    val (index, newRand) = rand.nextInt(size)
    val selectedCoord = lstOpenCoords(index)
    (selectedCoord, newRand)
  }

  def generateCoords(size: Int): List[Coord2D] = {
    List.range(0, size).foldRight(List[Coord2D]()) { (row, accRows) =>
      List.range(0, size).foldRight(accRows) { (col, accCols) =>
        (row, col) :: accCols
      }
    }
  }

  def printBoard(board: Board): Unit = {
    println("  " + board.indices.mkString(" "))
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

  def getUserMove(player: Stone.Value): Coord2D = {
    println(s"\nJogador ${if (player == Stone.Black) "Preto (B)" else "Branco (W)"}: Introduz as coordenadas (linha e coluna) separadas por espaço:")
    val input = scala.io.StdIn.readLine()
    val parts = input.trim.split(" ")
    if (parts.length != 2 || !parts.forall(_.forall(_.isDigit))) {
      println("Entrada inválida. Tenta novamente.")
      getUserMove(player)
    } else {
      (parts(0).toInt, parts(1).toInt)
    }
  }

  def play(board: Board, player: Stone.Value, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Board, List[Coord2D]) =
    if (!lstOpenCoords.contains(coord)) {
      println("Jogada inválida.")
      (board, lstOpenCoords)
    } else {
      val (r, c)    = coord
      val newRow    = board(r).updated(c, player)
      val newBoard  = board.updated(r, newRow)
      val newCoords = lstOpenCoords.filterNot(_ == coord)
      (newBoard, newCoords)
    }

  def captureGroupStones(board: Board, player: Stone.Value): (Board, Int) = {
    val opponent = if (player == Stone.Black) Stone.White else Stone.Black
    val size = board.length
    val visited = Array.fill(size, size)(false)

    def inBounds(r: Int, c: Int): Boolean =
      r >= 0 && c >= 0 && r < size && c < size

    def dfs(start: Coord2D): (Set[Coord2D], Boolean) = {
      val stack = collection.mutable.Stack(start)
      val group = collection.mutable.Set[Coord2D]()
      var hasLiberty = false

      while (stack.nonEmpty) {
        val (r, c) = stack.pop()
        if (!visited(r)(c)) {
          visited(r)(c) = true
          group += ((r, c))
          val neigh = List((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1))
          neigh.foreach { case (nr, nc) =>
            if (inBounds(nr, nc)) {
              board(nr)(nc) match {
                case Stone.Empty => hasLiberty = true
                case s if s == opponent && !visited(nr)(nc) => stack.push((nr, nc))
                case _ =>
              }
            }
          }
        }
      }
      (group.toSet, !hasLiberty)
    }

    val captured = collection.mutable.Set[Coord2D]()
    for {
      r <- 0 until size
      c <- 0 until size
      if board(r)(c) == opponent && !visited(r)(c)
    } {
      val (grp, surrounded) = dfs((r, c))
      if (surrounded) captured ++= grp
    }

    val newBoard = board.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (s, c) =>
        if (captured((r, c))) Stone.Empty else s
      }
    }

    (newBoard, captured.size)
  }

  def main(args: Array[String]): Unit = {
    val size = 9
    var board = List.fill(size)(List.fill(size)(Stone.Empty))
    var lstOpenCoords = generateCoords(size)
    var rand = MyRandom(42)
    var currentPlayer = Stone.Black

    println("=== Bem‑vindo ao jogo ===")
    println("Jogador Preto (B) vs Computador Branco (W)")

    while (lstOpenCoords.nonEmpty) {
      printBoard(board)

      val (coord, nextRand) =
        if (currentPlayer == Stone.Black) (getUserMove(currentPlayer), rand)
        else {
          val (pc, r2) = randomMove(lstOpenCoords, rand)
          println(s"\nComputador (W) jogou: ${pc._1} ${pc._2}")
          (pc, r2)
        }

      val (tmpBoard, tmpCoords) = play(board, currentPlayer, coord, lstOpenCoords)

      if (tmpBoard != board) {
        val (afterCapture, captured) = captureGroupStones(tmpBoard, currentPlayer)
        if (captured > 0) println(s"Capturadas $captured peças!")
        board = afterCapture
        lstOpenCoords = tmpCoords
        currentPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
        rand = nextRand
      }
    }

    printBoard(board)
    println("\nTabuleiro cheio. Fim de jogo.")
  }
}