import Game.{Board, Coord2D}
import javafx.fxml.FXML
import javafx.scene.control.{Button, TextField}
import javafx.scene.layout.BorderPane
class Controller {
  
  private var board: Board = Game.generateEmptyBoard(Game.size)
  private var rand: Game.MyRandom = Game.MyRandom(42)
  private var openCoords: List[Coord2D] = Game.generateCoords(Game.size)
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
  
  def onRandomPlayButtonClicked(): Unit = {
    Game.randomMove(openCoords, rand)
  }
  
  def onResetButtonClicked(): Unit = {
    Game.resetGame(Game.size, rand.seed)
  }
}

