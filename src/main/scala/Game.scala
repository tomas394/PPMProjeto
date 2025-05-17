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

  def main(args: Array[String]): Unit = {
    val size = 8
    var board: Board = List.fill(size)(List.fill(size)(Stone.Empty))
    var lstOpenCoords: List[Coord2D] = generateCoords(size)
    var rand = MyRandom(42)
    var currentPlayer = Stone.Black

    println("=== Bem-vindo ao jogo ===")
    println("Tu és o jogador Preto (B). O computador joga com as peças Brancas (W).")

    while (lstOpenCoords.nonEmpty) {
      printBoard(board)

      val (coord, updatedRand) =
        if (currentPlayer == Stone.Black) {
          val userCoord = getUserMove(currentPlayer)
          (userCoord, rand)
        } else {
          val (pcCoord, newRand) = randomMove(lstOpenCoords, rand)
          println(s"\nComputador (Branco - W) jogou em: ${pcCoord._1}, ${pcCoord._2}")
          (pcCoord, newRand)
        }

      val (newBoard, newCoords) = play(board, currentPlayer, coord, lstOpenCoords)

      // Apenas mudar de jogador se a jogada foi válida
      if (newBoard != board) {
        board = newBoard
        lstOpenCoords = newCoords
        currentPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
        rand = updatedRand
      }
    }

    printBoard(board)
    println("\nO tabuleiro está cheio. Fim do jogo.")
  }
}
