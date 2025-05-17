object Game {

  type Board = List[List[Stone.Value]]
  type Coord2D = (Int, Int)
  val CaptureLimit = 3

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

  def play(board: Board, player: Stone.Value, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Board, List[Coord2D]) = {
    if (!lstOpenCoords.contains(coord)) {
      println("Jogada inválida.")
      (board, lstOpenCoords)
    } else {
      val (row, col) = coord
      val newRow = board(row).updated(col, player)
      val newBoard = board.updated(row, newRow)
      val newLstOpenCoords = lstOpenCoords.filterNot(_ == coord)
      (newBoard, newLstOpenCoords)
    }
  }

  def captureGroupStones(board: Board, player: Stone.Value): (Board, Int) = {
    val size = board.length
    val visited = Array.fill(size, size)(false)
    var captured = Set.empty[Coord2D]
    val opponent = if (player == Stone.Black) Stone.White else Stone.Black

    def inBounds(r: Int, c: Int): Boolean =
      r >= 0 && c >= 0 && r < size && c < size

    def dfs(r: Int, c: Int): (Set[Coord2D], Boolean) = {
      val stack = collection.mutable.Stack((r, c))
      var group = Set.empty[Coord2D]
      var hasLiberty = false

      while (stack.nonEmpty) {
        val (x, y) = stack.pop()
        if (inBounds(x, y) && !visited(x)(y) && board(x)(y) == opponent) {
          visited(x)(y) = true
          group += ((x, y))

          val neighbors = List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
          for ((nx, ny) <- neighbors) {
            if (inBounds(nx, ny)) {
              board(nx)(ny) match {
                case Stone.Empty => hasLiberty = true
                case s if s == opponent && !visited(nx)(ny) =>
                  stack.push((nx, ny))
                case _ => // ignore
              }
            }
          }
        }
      }

      (group, hasLiberty)
    }

    for {
      r <- 0 until size
      c <- 0 until size
      if board(r)(c) == opponent && !visited(r)(c)
    } {
      val (group, hasLiberty) = dfs(r, c)
      if (!hasLiberty) {
        captured ++= group
      }
    }

    val newBoard = board.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (stone, c) =>
        if (captured.contains((r, c))) Stone.Empty else stone
      }
    }

    (newBoard, captured.size)
  }

  def checkWin(board: Board, player: Stone.Value): Boolean = {
    val size = board.length
    val visited = Array.fill(size, size)(false)

    def inBounds(r: Int, c: Int): Boolean =
      r >= 0 && c >= 0 && r < size && c < size

    def dfs(start: Coord2D): Boolean = {
      val stack = collection.mutable.Stack(start)
      var hasLiberty = false

      while (stack.nonEmpty) {
        val (r, c) = stack.pop()
        if (!visited(r)(c)) {
          visited(r)(c) = true
          val neighbors = List((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1))
          neighbors.foreach { case (nr, nc) =>
            if (inBounds(nr, nc)) {
              board(nr)(nc) match {
                case Stone.Empty => hasLiberty = true
                case s if s == player && !visited(nr)(nc) =>
                  stack.push((nr, nc))
                case _ => // do nothing
              }
            }
          }
        }
      }

      !hasLiberty
    }

    for {
      r <- 0 until size
      c <- 0 until size
      if board(r)(c) == player && !visited(r)(c)
    } {
      if (dfs((r, c))) return true
    }

    false
  }

  def main(args: Array[String]): Unit = {
    val size = 9
    var board: Board = List.fill(size)(List.fill(size)(Stone.Empty))
    var lstOpenCoords = generateCoords(size)
    var rand = MyRandom(42)
    var currentPlayer = Stone.Black
    var capturedBlack = 0 // peças que o Preto capturou
    var capturedWhite = 0 // peças que o Branco capturou
    var gameOver = false

    println("=== Bem‑vindo ao jogo ===")
    println("Preto (B) és tu; Computador é Branco (W)")
    println(s"Quem capturar $CaptureLimit peças primeiro vence.\n")

    while (lstOpenCoords.nonEmpty && !gameOver) {
      printBoard(board)

      /* --- escolher jogada --- */
      val (coord, nextRand) =
        if (currentPlayer == Stone.Black) (getUserMove(currentPlayer), rand)
        else {
          val (pc, r2) = randomMove(lstOpenCoords, rand)
          println(s"\nComputador (W) jogou: ${pc._1} ${pc._2}")
          (pc, r2)
        }

      val (tmpBoard, tmpCoords) = play(board, currentPlayer, coord, lstOpenCoords)

      if (tmpBoard != board) { // jogada válida
        val (afterCapture, capturedNow) = captureGroupStones(tmpBoard, currentPlayer)

        // actualizar contadores
        if (capturedNow > 0) {
          if (currentPlayer == Stone.Black) capturedBlack += capturedNow
          else capturedWhite += capturedNow
          println(s"Capturadas $capturedNow peça(s)!")
          println(s"Total capturas – Preto: $capturedBlack  |  Branco: $capturedWhite")
        }

        // verificar condição de vitória por capturas
        if (capturedBlack >= CaptureLimit) {
          println("\n*** Parabéns! Jogador Preto venceu por capturas! ***")
          gameOver = true
        } else if (capturedWhite >= CaptureLimit) {
          println("\n*** Computador (Branco) venceu por capturas! ***")
          gameOver = true
        } else {
          board = afterCapture
          lstOpenCoords = generateCoords(size).filter { case (r, c) => board(r)(c) == Stone.Empty }
          currentPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
          rand = nextRand
        }
      }
    }

    if (!gameOver) {
      printBoard(board)
      println("\nTabuleiro cheio. Fim do jogo (ninguém atingiu o limite de capturas).")
    }
  }
}
