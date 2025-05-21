object Game {

  type Board = List[List[Stone.Value]]
  type Coord2D = (Int, Int)
  val CaptureLimit = 5

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

  def getUserMove(player: Stone.Value, lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    println(s"\nJogador ${if (player == Stone.Black) "Preto (B)" else "Branco (W)"}: Introduz as coordenadas (linha e coluna), ou 'r' para jogada aleatória:")
    val input = scala.io.StdIn.readLine().trim

    if (input.toLowerCase == "r") {
      val (coord, newRand) = randomMove(lstOpenCoords, rand)
      println(s"Jogada aleatória: ${coord._1} ${coord._2}")
      (coord, newRand)
    } else {
      val parts = input.split(" ")
      if (parts.length != 2 || !parts.forall(_.forall(_.isDigit))) {
        println("Entrada inválida. Tenta novamente.")
        getUserMove(player, lstOpenCoords, rand)
      } else {
        ((parts(0).toInt, parts(1).toInt), rand)
      }
    }
  }

  def play(board: Board, player: Stone.Value, coord: Coord2D, lstOpenCoords: List[Coord2D], forbiddenCoords: Set[Coord2D]): (Board, List[Coord2D]) = {
    if (!lstOpenCoords.contains(coord) || forbiddenCoords.contains(coord)) {
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
    val opponent = if (player == Stone.Black) Stone.White else Stone.Black

    def inBounds(r: Int, c: Int): Boolean =
      r >= 0 && c >= 0 && r < size && c < size

    def dfs(r: Int, c: Int, visited: Set[Coord2D]): (Set[Coord2D], Boolean, Set[Coord2D]) = {
      def loop(stack: List[Coord2D], group: Set[Coord2D], hasLiberty: Boolean, visitedAcc: Set[Coord2D]): (Set[Coord2D], Boolean, Set[Coord2D]) = {
        stack match {
          case Nil => (group, hasLiberty, visitedAcc)
          case (x, y) :: rest =>
            if (!inBounds(x, y) || visitedAcc.contains((x, y)) || board(x)(y) != opponent)
              loop(rest, group, hasLiberty, visitedAcc)
            else {
              val newVisited = visitedAcc + ((x, y))
              val newGroup = group + ((x, y))
              val neighbors = List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
              val libertyFound = neighbors.exists { case (nx, ny) =>
                inBounds(nx, ny) && board(nx)(ny) == Stone.Empty
              }
              val nextStack = neighbors.filter { case (nx, ny) =>
                inBounds(nx, ny) && board(nx)(ny) == opponent && !newVisited.contains((nx, ny))
              } ::: rest
              loop(nextStack, newGroup, hasLiberty || libertyFound, newVisited)
            }
        }
      }

      loop(List((r, c)), Set(), false, visited)
    }

    def process(r: Int, c: Int, visited: Set[Coord2D], captured: Set[Coord2D]): (Set[Coord2D], Set[Coord2D]) = {
      if (visited.contains((r, c)) || board(r)(c) != opponent)
        (visited, captured)
      else {
        val (group, hasLiberty, newVisited) = dfs(r, c, visited)
        if (!hasLiberty)
          (newVisited, captured ++ group)
        else
          (newVisited, captured)
      }
    }

    def iterateAll(r: Int, c: Int, visited: Set[Coord2D], captured: Set[Coord2D]): (Set[Coord2D], Set[Coord2D]) = {
      if (r >= size) (visited, captured)
      else if (c >= size) iterateAll(r + 1, 0, visited, captured)
      else {
        val (newVisited, newCaptured) = process(r, c, visited, captured)
        iterateAll(r, c + 1, newVisited, newCaptured)
      }
    }

    val (_, capturedStones) = iterateAll(0, 0, Set(), Set())

    val newBoard = board.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (stone, c) =>
        if (capturedStones.contains((r, c))) Stone.Empty else stone
      }
    }

    (newBoard, capturedStones.size)
  }


  def main(args: Array[String]): Unit = {
    val size = 9
    var board: Board = List.fill(size)(List.fill(size)(Stone.Empty))
    var lstOpenCoords = generateCoords(size)
    var rand = MyRandom(42)
    var currentPlayer = Stone.Black
    var capturedBlack = 0
    var capturedWhite = 0
    var forbiddenCoords = Set.empty[Coord2D]
    var gameOver = false

    println("=== Bem‑vindo ao jogo ===")
    println("Preto (B) és tu; Computador é Branco (W)")
    println(s"Quem capturar $CaptureLimit peças primeiro vence.\n")

    while (lstOpenCoords.nonEmpty && !gameOver) {
      printBoard(board)

      val (coord, nextRand) =
        if (currentPlayer == Stone.Black)
          getUserMove(currentPlayer, lstOpenCoords.filterNot(forbiddenCoords.contains), rand)
        else {
          val validCoords = lstOpenCoords.filterNot(forbiddenCoords.contains)
          val (pc, r2) = randomMove(validCoords, rand)
          println(s"\nComputador (W) jogou: ${pc._1} ${pc._2}")
          (pc, r2)
        }

      val (tmpBoard, tmpCoords) = play(board, currentPlayer, coord, lstOpenCoords, forbiddenCoords)

      if (tmpBoard != board) {
        val (afterCapture, capturedNow) = captureGroupStones(tmpBoard, currentPlayer)

        if (capturedNow > 0) {
          val capturedPositions = for {
            r <- 0 until size
            c <- 0 until size
            if board(r)(c) != Stone.Empty && afterCapture(r)(c) == Stone.Empty
          } yield (r, c)
          forbiddenCoords ++= capturedPositions.toSet

          if (currentPlayer == Stone.Black) capturedBlack += capturedNow
          else capturedWhite += capturedNow

          println(s"Capturadas $capturedNow peça(s)!")
          println(s"Total capturas – Preto: $capturedBlack  |  Branco: $capturedWhite")
        }

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
