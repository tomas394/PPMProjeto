import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import java.util.concurrent.TimeoutException

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

  case class GameState(
                        board: Board,
                        openCoords: List[Coord2D],
                        rand: MyRandom,
                        currentPlayer: Stone.Value,
                        capturedBlack: Int,
                        capturedWhite: Int,
                        forbiddenCoords: Set[Coord2D]
                      )

  def askForInt(prompt: String, default: Int, min: Int = 1): Int = {
    println(s"$prompt (padrão: $default):")
    val input = scala.io.StdIn.readLine().trim
    if (input.isEmpty) default
    else {
      try {
        val value = input.toInt
        if (value < min) {
          println(s"Por favor, insira um número maior ou igual a $min.")
          askForInt(prompt, default, min)
        } else value
      } catch {
        case _: NumberFormatException =>
          println("Entrada inválida. Tente novamente.")
          askForInt(prompt, default, min)
      }
    }
  }

  def generateCoords(size: Int): List[Coord2D] =
    List.range(0, size).flatMap(row => List.range(0, size).map(col => (row, col)))

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

  def getUserMove(player: Stone.Value, lstOpenCoords: List[Coord2D], rand: MyRandom, timeLimit: Int, capturedBlack: Int, capturedWhite: Int): (Option[Coord2D], MyRandom, Boolean)
  = {
    println(s"\nJogador ${if (player == Stone.Black) "Preto (B)" else "Branco (W)"}: Introduz coordenadas (linha coluna), 'r' para aleatória, 'undo', 'reset' ou 'capturas':")
    println(s"Tens ${timeLimit} segundos para jogar...")

    def prompt(): (Option[Coord2D], MyRandom, Boolean) = {
      val futureInput = Future {
        scala.io.StdIn.readLine().trim
      }

      try {
        val input = Await.result(futureInput, Duration(timeLimit, SECONDS)).toLowerCase
        input match {
          case "r" =>
            val (coord, newRand) = randomMove(lstOpenCoords, rand)
            println(s"Jogada aleatória: ${coord._1} ${coord._2}")
            (Some(coord), newRand, false)
          case "undo" =>
            (None, rand, false)
          case "reset" =>
            (None, rand, true)
          case "capturas" =>
            println(s"Capturas | Preto (B): $capturedBlack | Branco (W): $capturedWhite")
            prompt()
          case _ =>
            val parts = input.split(" ")
            if (parts.length != 2 || !parts.forall(_.forall(_.isDigit))) {
              println("Entrada inválida.")
              prompt()
            } else {
              ((Some((parts(0).toInt, parts(1).toInt)), rand, false))
            }
        }
      } catch {
        case _: TimeoutException =>
          println("\nTempo esgotado. Jogada aleatória será feita.")
          val (coord, newRand) = randomMove(lstOpenCoords, rand)
          println(s"Jogada aleatória: ${coord._1} ${coord._2}")
          (Some(coord), newRand, false)
      }
    }

    prompt()
  }


  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val size = lstOpenCoords.size
    val (index, newRand) = rand.nextInt(size)
    val selectedCoord = lstOpenCoords(index)
    (selectedCoord, newRand)
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

    def inBounds(r: Int, c: Int): Boolean = r >= 0 && c >= 0 && r < size && c < size

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

  def generateEmptyBoard(size: Int): Board =
    List.fill(size)(List.fill(size)(Stone.Empty))

  def undo(currentState: GameState, history: List[GameState]): Option[(GameState, List[GameState])] = {
    history match {
      case computerState :: playerState :: rest =>
        Some((playerState.copy(), rest))
      case playerState :: Nil =>
        Some((playerState.copy(), Nil))
      case Nil =>
        None
    }
  }

  def run(size: Int, board: Board, openCoords: List[Coord2D], rand: MyRandom,
          currentPlayer: Stone.Value, capturedBlack: Int, capturedWhite: Int,
          forbiddenCoords: Set[Coord2D], captureLimit: Int, timeLimit: Int): Unit = {
    println("=== Bem‑vindo ao jogo ===")
    println("Preto (B) és tu; Computador é Branco (W)")

    val initialState = GameState(
      board = board,
      openCoords = openCoords,
      rand = rand,
      currentPlayer = currentPlayer,
      capturedBlack = capturedBlack,
      capturedWhite = capturedWhite,
      forbiddenCoords = forbiddenCoords
    )

    gameLoop(initialState, Nil, size, captureLimit, timeLimit)
  }

  def gameLoop(state: GameState, history: List[GameState], size: Int, captureLimit: Int, timeLimit: Int): Unit = {
    val newHistory = state :: history
    if (state.openCoords.isEmpty) {
      printBoard(state.board)
      println("\nTabuleiro cheio. Fim do jogo (ninguém atingiu o limite de capturas).")
      return
    }

    printBoard(state.board)
    println(s"Capturas: Preto = ${state.capturedBlack}, Branco = ${state.capturedWhite}")

    if (state.capturedBlack >= captureLimit) {
      println("\n*** Parabéns! Jogador Preto venceu por capturas! ***")
      return
    } else if (state.capturedWhite >= captureLimit) {
      println("\n*** Computador (Branco) venceu por capturas! ***")
      return
    }

    val nextState = if (state.currentPlayer == Stone.Black) {
      val (coordOpt, nextRand, wantsReset) = getUserMove(state.currentPlayer, state.openCoords.filterNot(state.forbiddenCoords.contains), state.rand, timeLimit, state.capturedBlack, state.capturedWhite)

      if (wantsReset) {
        println("\n*** Jogo reiniciado! ***\n")
        val resetBoard = generateEmptyBoard(size)
        val resetCoords = generateCoords(size)
        val resetRand = MyRandom(state.rand.seed)
        val resetState = GameState(resetBoard, resetCoords, resetRand, Stone.Black, 0, 0, Set.empty)
        return gameLoop(resetState, Nil, size, captureLimit, timeLimit)
      }

      coordOpt match {
        case None =>
          undo(state, history) match {
            case Some((prev, newHist)) =>
              println("Última jogada anulada (jogador e computador).")
              gameLoop(prev, newHist, size, captureLimit, timeLimit)
            case None =>
              println("Não há jogadas anteriores para anular.")
              gameLoop(state, history, size, captureLimit, timeLimit)
          }
        case Some(coord) =>
          val newHistory = state :: history
          val (tmpBoard, tmpCoords) = play(state.board, state.currentPlayer, coord, state.openCoords, state.forbiddenCoords)
          if (tmpBoard != state.board) {
            val (afterCapture, capturedNow) = captureGroupStones(tmpBoard, state.currentPlayer)
            val capturedPositions = for {
              r <- 0 until size
              c <- 0 until size
              if state.board(r)(c) != Stone.Empty && afterCapture(r)(c) == Stone.Empty
            } yield (r, c)
            val updatedState = state.copy(
              board = afterCapture,
              openCoords = generateCoords(size).filter { case (r, c) => afterCapture(r)(c) == Stone.Empty },
              rand = nextRand,
              currentPlayer = Stone.White,
              capturedBlack = state.capturedBlack + capturedNow,
              forbiddenCoords = state.forbiddenCoords ++ capturedPositions
            )
            gameLoop(updatedState, newHistory, size, captureLimit, timeLimit)
          } else {
            println("Jogada inválida.")
            gameLoop(state, history, size, captureLimit, timeLimit)
          }
      }
    } else {
      val validCoords = state.openCoords.filterNot(state.forbiddenCoords.contains)
      val (coord, nextRand) = randomMove(validCoords, state.rand)
      println(s"\nComputador (W) jogou: ${coord._1} ${coord._2}")

      val (tmpBoard, tmpCoords) = play(state.board, state.currentPlayer, coord, state.openCoords, state.forbiddenCoords)
      if (tmpBoard != state.board) {
        val (afterCapture, capturedNow) = captureGroupStones(tmpBoard, state.currentPlayer)
        val capturedPositions = for {
          r <- 0 until size
          c <- 0 until size
          if state.board(r)(c) != Stone.Empty && afterCapture(r)(c) == Stone.Empty
        } yield (r, c)
        val updatedState = state.copy(
          board = afterCapture,
          openCoords = generateCoords(size).filter { case (r, c) => afterCapture(r)(c) == Stone.Empty },
          rand = nextRand,
          currentPlayer = Stone.Black,
          capturedWhite = state.capturedWhite + capturedNow,
          forbiddenCoords = state.forbiddenCoords ++ capturedPositions
        )
        gameLoop(updatedState, state :: history, size, captureLimit, timeLimit)
      } else {
        gameLoop(state, history, size, captureLimit, timeLimit)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println("=== Configuração do Jogo ===")

    val size = askForInt("Tamanho do tabuleiro (ex: 9 para 9x9)", 9, 3)
    val turnTimeLimit = askForInt("Tempo máximo por jogada (em segundos)", 15)
    val captureLimit = askForInt("Número de peças capturadas para vencer", 3)

    val board = generateEmptyBoard(size)
    val openCoords = generateCoords(size)
    val rand = MyRandom(42)

    run(size, board, openCoords, rand, Stone.Black, 0, 0, Set.empty, captureLimit, turnTimeLimit)
  }
}
