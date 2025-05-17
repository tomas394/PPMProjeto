object Game {

  type Board = List[List[Stone.Value]]
  type Coord2D = (Int, Int)


  object Stone extends Enumeration {
    type Stone = Value
    val Black, White, Empty = Value // Peças pretas, brancas e espaços vazios
  }

  // Gerador de números aleatórios
  case class MyRandom(seed: Long) {
    def nextInt(n: Int): (Int, MyRandom) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRand = MyRandom(newSeed)
      val number = ((newSeed >>> 16).toInt.abs) % n
      (number, nextRand)
    }
  }

  // Função para escolher uma jogada aleatória dentro da lista de posições disponíveis                        T1
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val size = lstOpenCoords.size
    val (index, newRand) = rand.nextInt(size)
    val selectedCoord = lstOpenCoords(index)
    (selectedCoord, newRand)
  }

  // Função para realizar uma jogada                    T2
  def play(board: Board, player: Stone.Value, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    if (!lstOpenCoords.contains(coord)) {
      (None, lstOpenCoords) // Retorna None se a jogada não for válida
    } else {
      val (row, col) = coord
      val newRow = board(row).updated(col, player) // Atualiza a linha com a jogada do jogador
      val newBoard = board.updated(row, newRow) // Atualiza o tabuleiro
      val newLstOpenCoords = lstOpenCoords.filterNot(_ == coord) // Remove a posição ocupada
      (Some(newBoard), newLstOpenCoords)
    }
  }

  def playRandomly(board: Board, r: MyRandom, player: Stone.Value, lstOpenCoords: List[Coord2D], // T3
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Board, MyRandom, List[Coord2D]) = {
    // Obtém uma coordenada aleatória válida para a jogada
    val (coord, newRand) = f(lstOpenCoords, r)

    // Tenta jogar nessa coordenada
    val (maybeNewBoard, newLstOpenCoords) = play(board, player, coord, lstOpenCoords)

    // Retorna o novo tabuleiro se a jogada for válida, caso contrário mantém o estado atual
    maybeNewBoard match {
      case Some(newBoard) => (newBoard, newRand, newLstOpenCoords)
      case None => (board, r, lstOpenCoords)
    }
  }


  // Função para imprimir o tabuleiro no console                               T4
  def printBoard(board: Board): Unit = {
    println("  " + board.indices.mkString(" ")) // Imprime cabeçalho de colunas
    for ((row, i) <- board.zipWithIndex) {
      print(i + " ") // Imprime índice da linha
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

  def generateCoords(size: Int): List[Coord2D] = { //FOLD RIGHT
    List.range(0, size).foldRight(List[Coord2D]()) { (row, accRows) =>
      List.range(0, size).foldRight(accRows) { (col, accCols) =>
        (row, col) :: accCols
      }
    }
  }

  def captureGroupStones(board: Board, player: Stone.Value): (Board , Int) = {
    
  }

  def main(args: Array[String]): Unit = {
    val size = 3 // Define o tamanho do tabuleiro 3x3
    val initialBoard: Board = List.fill(size)(List.fill(size)(Stone.Empty)) // Cria um tabuleiro vazio

    // Cria a lista de coordenadas disponíveis usando foldRight
    val initialLstOpenCoords: List[Coord2D] = generateCoords(size)

    val rand = MyRandom(42) // Inicializa o gerador aleatório

    // Jogador Preto joga na posição (1,1)
    val (board1, openCoords1) = play(initialBoard, Stone.Black, (1, 1), initialLstOpenCoords)
    printBoard(board1.getOrElse(initialBoard))

    println()
    // Jogador Branco joga na posição (0,0)
    val (board2, openCoords2) = play(board1.getOrElse(initialBoard), Stone.White, (0, 0), openCoords1)
    printBoard(board2.getOrElse(initialBoard))
  }


}


// A função getOrElse é usada para obter o valor dentro de um Option, ou um valor padrão caso seja None.
// No contexto do jogo, play pode falhar (retornando None) se a jogada for inválida.
// Para evitar erros ao tentar acessar um tabuleiro que não existe, usamos getOrElse para garantir um valor seguro.