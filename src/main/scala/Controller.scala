import Game.{Board, Coord2D}
import javafx.fxml.FXML
import javafx.scene.control.{Button, TextField}
import javafx.scene.layout.{BorderPane, ColumnConstraints, GridPane, RowConstraints, StackPane}
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
class Controller {
  
  private val defaultSize = 19 
  private var board: Board = Game.generateEmptyBoard(defaultSize)
  private var rand: Game.MyRandom = Game.MyRandom(42)
  private var openCoords: List[Coord2D] = Game.generateCoords(defaultSize)
  private var currentPlayer: Game.Stone.Value = Game.Stone.Black
  private var forbidden: Set[Coord2D] = Set.empty

  @FXML
  private var borderPane: BorderPane = borderPane
  
  @FXML
  private var randomPlayButton: Button = randomPlayButton

  @FXML
  private var resetButton: Button = resetButton

  @FXML
  private var undoButton: Button = undoButton

  @FXML
  private var boardGrid: GridPane = boardGrid
  
  def onRandomPlayButtonClicked(): Unit = {
    Game.randomMove(openCoords, rand)
  }

  def onResetButtonClicked(): Unit = {
    board = Game.generateEmptyBoard(defaultSize)
    rand = Game.MyRandom(rand.seed)
    openCoords = Game.generateCoords(defaultSize)
    currentPlayer = Game.Stone.Black
    forbidden = Set.empty
    createBoardUI(defaultSize)
  }

/*  def onUndoButtonClicked(): Unit = {

  }*/

  def createBoardUI(size: Int): Unit = {
    boardGrid.getChildren.clear()
    boardGrid.getColumnConstraints.clear()
    boardGrid.getRowConstraints.clear()

    // Define tamanho das células
    for (_ <- 0 until size) {
      val col = new ColumnConstraints()
      col.setPercentWidth(100.0 / size)
      boardGrid.getColumnConstraints.add(col)

      val row = new RowConstraints()
      row.setPercentHeight(100.0 / size)
      boardGrid.getRowConstraints.add(row)
    }

    for (row <- 0 until size; col <- 0 until size) {
      val cell = new StackPane()
      cell.getStyleClass.add("board-cell") // define no CSS se quiser

      // Evento de clique na célula (por exemplo)
      cell.setOnMouseClicked(_ => handleCellClick(row, col))

      boardGrid.add(cell, col, row)
    }
  }

  def handleCellClick(row: Int, col: Int): Unit = {
    println(s"Jogador clicou na célula ($row, $col)")

    // Aqui podes verificar se a célula está disponível e fazer a jogada:
    val coord = (row, col)

    // Chamar o Game para realizar a jogada (adaptar ao teu modelo)
   // if (!currentState.forbiddenCoords.contains(coord) && currentState.openCoords.contains(coord)) {
   //   val (newBoard, newOpenCoords) = Game.play(currentState.board, currentState.currentPlayer, coord, currentState.openCoords, currentState.forbiddenCoords)

      // Atualiza o estado atual com a nova board
    //  currentState = currentState.copy(
     //   board = newBoard,
     //   openCoords = newOpenCoords,
     //   currentPlayer = if (currentState.currentPlayer == Game.Stone.Black) Game.Stone.White else Game.Stone.Black
     // )

      // Redesenhar o tabuleiro
   //   updateBoardUI()
   // } else {
    //  println("Jogada inválida ou célula já ocupada.")
   // }
  }


  def drawPiece(row: Int, col: Int, color: Color): Unit = {
    val circle = new Circle(10, color)
    val cell = getCell(row, col)
    cell.getChildren.clear()
    cell.getChildren.add(circle)
  }

  def getCell(row: Int, col: Int): StackPane = {
    boardGrid.getChildren.filtered {
      case node: StackPane =>
        GridPane.getRowIndex(node) == row && GridPane.getColumnIndex(node) == col
      case _ => false
    }.get(0).asInstanceOf[StackPane]
  }

}

