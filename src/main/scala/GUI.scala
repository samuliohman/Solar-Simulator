import scala.swing._
import scala.swing.event.ButtonClicked

object GUI extends SimpleSwingApplication {
  val nappula = new Button("Nappi")
  val textField = new TextField("Default input", 50)
  val textArea = new TextArea("Default text", 90, 90)

  val horizontalPanel = new BoxPanel(Orientation.Horizontal)
  horizontalPanel.contents += new Label("Here we have a text box")
  horizontalPanel.contents += textField
  horizontalPanel.contents += nappula

  val fullPanel = new BoxPanel(Orientation.Vertical)
  fullPanel.contents += horizontalPanel
  fullPanel.contents += textArea
  fullPanel.preferredSize = new Dimension(1280,720)

  val window = new Frame
  window.title = "Solar simulator"
  window.contents = fullPanel

  this.listenTo(nappula)
  this.reactions += {
    case painallus: ButtonClicked =>
      val lahdenappula = painallus.source
      val textInBox = textField.text
      textArea.text = textInBox
  }

  override def top: Frame = window
}
