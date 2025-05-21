import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.layout.{GridPane, StackPane, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
import javafx.scene.control.Label
import javafx.stage.Stage

import Game._

class GameUI extends Application {

  val size = 9
  var board: Board = List.fill(size)(List.fill(size)(Stone.Empty))
  var lstOpenCoords = generateCoords(size)
  var currentPlayer = Stone.Black
  var capturedBlack = 0
  var capturedWhite = 0
  var forbiddenCoords = Set.empty[Coord2D]
  var rand = MyRandom(42)

  val statusLabel = new Label("Jogador: Preto (B)")
  val grid = new GridPane()

  override def start(primaryStage: Stage): Unit = {
    drawBoard()

    val root = new VBox(10, statusLabel, grid)
    val scene = new Scene(root)
    primaryStage.setScene(scene)
    primaryStage.setTitle("AtariGo")
    primaryStage.show()
  }

  def drawBoard(): Unit = {
    grid.getChildren.clear()

    for (i <- 0 until size; j <- 0 until size) {
      val cell = new StackPane()
      cell.setPrefSize(40, 40)
      cell.setStyle("-fx-border-color: black; -fx-background-color: beige;")

      val x = i
      val y = j

      board(x)(y) match {
        case Stone.Black =>
          val stone = new Circle(15, Color.BLACK)
          cell.getChildren.add(stone)
        case Stone.White =>
          val stone = new Circle(15, Color.WHITE)
          stone.setStroke(Color.BLACK)
          cell.getChildren.add(stone)
        case _ => // empty
      }

      cell.setOnMouseClicked(_ => {
        handleMove((x, y))
      })

      grid.add(cell, y, x)
    }
  }

  def handleMove(coord: Coord2D): Unit = {
    if (currentPlayer == Stone.White) return // impedir jogada manual do branco (CPU)

    val (newBoard, newOpen) = play(board, currentPlayer, coord, lstOpenCoords, forbiddenCoords)

    if (newBoard != board) {
      val (afterCapture, capturedNow) = captureGroupStones(newBoard, currentPlayer)
      if (capturedNow > 0) {
        val capturedPositions = for {
          r <- 0 until size
          c <- 0 until size
          if board(r)(c) != Stone.Empty && afterCapture(r)(c) == Stone.Empty
        } yield (r, c)
        forbiddenCoords ++= capturedPositions.toSet

        capturedBlack += capturedNow
        statusLabel.setText(s"Capturadas $capturedNow peça(s)! Total Preto: $capturedBlack")
      }

      board = afterCapture
      lstOpenCoords = generateCoords(size).filter { case (r, c) => board(r)(c) == Stone.Empty }

      if (capturedBlack >= CaptureLimit) {
        statusLabel.setText("Preto venceu por capturas!")
        return
      }

      currentPlayer = Stone.White
      drawBoard()
      makeCPUMove()
    }
  }

  def makeCPUMove(): Unit = {
    val validCoords = lstOpenCoords.filterNot(forbiddenCoords.contains)
    if (validCoords.isEmpty) return

    val (coord, nextRand) = randomMove(validCoords, rand)
    rand = nextRand

    val (newBoard, newOpen) = play(board, currentPlayer, coord, lstOpenCoords, forbiddenCoords)
    if (newBoard != board) {
      val (afterCapture, capturedNow) = captureGroupStones(newBoard, currentPlayer)

      if (capturedNow > 0) {
        val capturedPositions = for {
          r <- 0 until size
          c <- 0 until size
          if board(r)(c) != Stone.Empty && afterCapture(r)(c) == Stone.Empty
        } yield (r, c)
        forbiddenCoords ++= capturedPositions.toSet

        capturedWhite += capturedNow
        statusLabel.setText(s"Computador capturou $capturedNow! Total Branco: $capturedWhite")
      }

      board = afterCapture
      lstOpenCoords = generateCoords(size).filter { case (r, c) => board(r)(c) == Stone.Empty }

      if (capturedWhite >= CaptureLimit) {
        statusLabel.setText("Computador venceu por capturas!")
        return
      }

      currentPlayer = Stone.Black
      statusLabel.setText("Jogador: Preto (B)")
      drawBoard()
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[GameUI], args: _*)
  }
}

