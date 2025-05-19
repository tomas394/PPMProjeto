import javafx.fxml.FXML
import javafx.scene.control.{Button, TextField}
class Controller {

  @FXML
  private var button1: Button = _

  @FXML
  private var textField1: TextField = _


  def onButton1Clicked(): Unit = {
    textField1.setText("Hello World")
  }
}

