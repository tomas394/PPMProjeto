import Game.Stone.Stone

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import java.util.concurrent.TimeoutException

object Game {

  type Board = List[List[Stone.Value]]
  type Coord2D = (Int, Int)
  val CaptureLimit = 5
  val TurnTimeLimitMs = 15000

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
                        currentPlayer: Stone,
                        capturedBlack: Int,
                        capturedWhite: Int,
                        forbiddenCoords: Set[Coord2D]
                      )

  def tryUndo(history: List[GameState]): Option[(GameState, List[GameState])] = {
    history.headOption match {
      case Some(state) => Some((state, history.drop(1)))
      case None        => None
    }
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val size = lstOpenCoords.size
    val (index, newRand) = rand.nextInt(size)
    val selectedCoord = lstOpenCoords(index)
    (selectedCoord, newRand)
  }

  def generateCoords(size: Int): List[Coord2D] = {
    List.range(0, size).flatMap { row =>
      List.range(0, size).map { col => (row, col) }
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

  def getUserMove(player: Stone.Value, lstOpenCoords: List[Coord2D], rand: MyRandom): (Option[Coord2D], MyRandom) = {
    println(s"\nJogador ${if (player == Stone.Black) "Preto (B)" else "Branco (W)"}: Introduz coordenadas (linha coluna), 'r' para aleatória, ou 'undo':")
    println(s"Tens ${TurnTimeLimitMs / 1000} segundos para jogar...")

    val futureInput = Future {
      scala.io.StdIn.readLine().trim
    }

    try {
      val input = Await.result(futureInput, Duration(TurnTimeLimitMs, MILLISECONDS)).toLowerCase
      input match {
        case "r" =>
          val (coord, newRand) = randomMove(lstOpenCoords, rand)
          println(s"Jogada aleatória: ${coord._1} ${coord._2}")
          (Some(coord), newRand)
        case "undo" =>
          (None, rand)
        case _ =>
          val parts = input.split(" ")
          if (parts.length != 2 || !parts.forall(_.forall(_.isDigit))) {
            println("Entrada inválida.")
            getUserMove(player, lstOpenCoords, rand)
          } else {
            ((Some((parts(0).toInt, parts(1).toInt)), rand))
          }
      }
    } catch {
      case _: TimeoutException =>
        println("\nTempo esgotado. Jogada aleatória será feita.")
        val (coord, newRand) = randomMove(lstOpenCoords, rand)
        println(s"Jogada aleatória: ${coord._1} ${coord._2}")
        (Some(coord), newRand)
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

  def run(
           size: Int,
           board: List[List[Stone]],
           lstOpenCoords: List[Coord2D],
           rand: MyRandom,
           currentPlayer: Stone,
           capturedBlack: Int,
           capturedWhite: Int,
           forbiddenCoords: Set[Coord2D]
         ): Unit = {

    var gameBoard = board
    var openCoords = lstOpenCoords
    var random = rand
    var player = currentPlayer
    var blackCaptures = capturedBlack
    var whiteCaptures = capturedWhite
    var forbidden = forbiddenCoords
    var gameOver = false

    var history = List.empty[GameState]

    println("=== Bem‑vindo ao jogo ===")
    println("Preto (B) és tu; Computador é Branco (W)")
    println(s"Quem capturar $CaptureLimit peças primeiro vence.\n")

    while (openCoords.nonEmpty && !gameOver) {
      printBoard(gameBoard)

      if (player == Stone.Black) {
        val (coordOpt, nextRand) = getUserMove(player, openCoords.filterNot(forbidden.contains), random)

        if (coordOpt.isEmpty) {
          tryUndo(history) match {
            case Some((prev, newHist)) =>
              gameBoard = prev.board
              openCoords = prev.openCoords
              random = prev.rand
              player = prev.currentPlayer
              blackCaptures = prev.capturedBlack
              whiteCaptures = prev.capturedWhite
              forbidden = prev.forbiddenCoords
              history = newHist
              println("Última jogada anulada (jogador e computador).")
            case None =>
              println("Não há jogadas anteriores para anular.")
          }
        } else {
          history = List(GameState(gameBoard, openCoords, random, player, blackCaptures, whiteCaptures, forbidden))
          val coord = coordOpt.get
          val (tmpBoard, tmpCoords) = play(gameBoard, player, coord, openCoords, forbidden)
          if (tmpBoard != gameBoard) {
            val (afterCapture, capturedNow) = captureGroupStones(tmpBoard, player)
            if (capturedNow > 0) {
              val capturedPositions = for {
                r <- 0 until size
                c <- 0 until size
                if gameBoard(r)(c) != Stone.Empty && afterCapture(r)(c) == Stone.Empty
              } yield (r, c)
              forbidden ++= capturedPositions.toSet
              blackCaptures += capturedNow
              println(s"Capturadas $capturedNow peça(s)!")
            }
            gameBoard = afterCapture
            openCoords = generateCoords(size).filter { case (r, c) => gameBoard(r)(c) == Stone.Empty }
            player = Stone.White
            random = nextRand
          }
        }
      } else {
        val validCoords = openCoords.filterNot(forbidden.contains)
        val (coord, nextRand) = randomMove(validCoords, random)
        println(s"\nComputador (W) jogou: ${coord._1} ${coord._2}")
        val (tmpBoard, tmpCoords) = play(gameBoard, player, coord, openCoords, forbidden)
        if (tmpBoard != gameBoard) {
          val (afterCapture, capturedNow) = captureGroupStones(tmpBoard, player)
          if (capturedNow > 0) {
            val capturedPositions = for {
              r <- 0 until size
              c <- 0 until size
              if gameBoard(r)(c) != Stone.Empty && afterCapture(r)(c) == Stone.Empty
            } yield (r, c)
            forbidden ++= capturedPositions.toSet
            whiteCaptures += capturedNow
            println(s"Capturadas $capturedNow peça(s)!")
          }
          gameBoard = afterCapture
          openCoords = generateCoords(size).filter { case (r, c) => gameBoard(r)(c) == Stone.Empty }
          player = Stone.Black
          random = nextRand
        }
      }

      if (blackCaptures >= CaptureLimit) {
        println("\n*** Parabéns! Jogador Preto venceu por capturas! ***")
        gameOver = true
      } else if (whiteCaptures >= CaptureLimit) {
        println("\n*** Computador (Branco) venceu por capturas! ***")
        gameOver = true
      }
    }

    if (!gameOver) {
      printBoard(gameBoard)
      println("\nTabuleiro cheio. Fim do jogo (ninguém atingiu o limite de capturas).")
    }
  }

  def main(args: Array[String]): Unit = {
    val size = 9
    val board: Board = List.fill(size)(List.fill(size)(Stone.Empty))
    val lstOpenCoords = generateCoords(size)
    val rand = MyRandom(42)
    val currentPlayer = Stone.Black
    val capturedBlack = 0
    val capturedWhite = 0
    val forbiddenCoords = Set.empty[Coord2D]

    run(size, board, lstOpenCoords, rand, currentPlayer, capturedBlack, capturedWhite, forbiddenCoords)
  }
}
