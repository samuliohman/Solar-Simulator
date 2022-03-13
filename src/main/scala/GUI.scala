import java.awt.{Color, RenderingHints}
import javax.swing.SwingConstants
import javax.swing.text.Position
import scala.swing._

object GUI extends SimpleSwingApplication {
  //Creates the left segment of the screen where there are buttons and text boxes
  def createLeftBar: BoxPanel = {
    val dropDown = new ComboBox(Seq("body1", "body2", "body3"))
    val textField = new TextField("Default input", 25)
    val dropDown2 = new ComboBox(Seq("thing1", "thing2", "thing3"))
    val button = new Button("Random button")

    val verticalPanel = new BoxPanel(Orientation.Vertical)
    verticalPanel.contents += new Label("Left bar here")
    verticalPanel.contents += dropDown
    verticalPanel.contents += textField
    verticalPanel.contents += dropDown2
    verticalPanel.contents += button

    verticalPanel
  }

  //Creates the upper segment of the screen where there are buttons and text boxes
  def createTopBar = {
    val labelFontSize = 30
    //Creating the buttons and text box concerning camera angles
    val camLabel = new Label("Camera angle")
    camLabel.peer.setHorizontalAlignment(SwingConstants.CENTER)
    camLabel.peer.setFont(camLabel.peer.getFont.deriveFont(0,labelFontSize))
    val camButton1 = new Button("xy -plane")
    val camButton2 = new Button("xz -plane")
    val camButton3 = new Button("yz -plane")

    val camButtons = new BoxPanel(Orientation.Horizontal)
    camButtons.contents += camButton1
    camButtons.contents += camButton2
    camButtons.contents += camButton3

    val camPanel = new BoxPanel(Orientation.Vertical)
    camPanel.contents += camLabel
    camPanel.contents += camButtons

    //Creating the buttons and text box concerning the adjustment of time step
    val timeStepLabel = new Label("Time step")
    timeStepLabel.peer.setFont(timeStepLabel.peer.getFont.deriveFont(0,labelFontSize))
    val timeStepTextField = new TextField("Please input a double", 25)
    val timeStepButton = new Button("Apply time step")

    val timeStepElements = new BoxPanel(Orientation.Horizontal)
    timeStepElements.contents += timeStepTextField
    timeStepElements.contents += timeStepButton

    val timeStepPanel = new BoxPanel(Orientation.Vertical)
    timeStepPanel.contents += timeStepLabel
    timeStepPanel.contents += timeStepElements

    //Adds all the aforementioned elements to one panel
    val horizontalPanel = new FlowPanel
    horizontalPanel.contents += camPanel
    horizontalPanel.contents += timeStepPanel

    horizontalPanel
  }

  override def top: Frame = new MainFrame {
    //Dimensios of of the simulation screen (which doesn't contain left and top bar)
    val simulationWidth = 1280
    val simulationHeight = 720

    /** Creates the part of the screen where the simulations occurs */
    val simulationScreen = new Panel {

      //Overriding the default method enables us to draw our own graphics.
      override def paintComponent(g: Graphics2D) = {

        // Paint on the old image a rectangle with bright blue (red=80, green=180, blue=235)
        g.setColor(new Color(80, 180, 235))
        g.fillRect(0, 0, simulationWidth, simulationHeight)

        // Ask Graphics2D to provide us smoother graphics, i.e., antialising
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

        // change paint color to white and ask the space to redraw itself.
        g.setColor(Color.white)
      }
    }
    //Sets the size of the simulation screen (which doesn't contain left and top bar)
    simulationScreen.minimumSize = new Dimension(simulationWidth, simulationHeight)
    simulationScreen.preferredSize = new Dimension(simulationWidth, simulationHeight)
    simulationScreen.maximumSize = new Dimension(simulationWidth, simulationHeight)

    //Creates a panel consisting all the different elements
    val topAndSim = new BoxPanel(Orientation.Vertical)
    topAndSim.contents += createTopBar
    topAndSim.contents += simulationScreen

    val fullPanel = new BoxPanel(Orientation.Horizontal)
    fullPanel.contents += createLeftBar
    fullPanel.contents += topAndSim

    //Adds the panel that has all the different elements to current window
    contents = fullPanel
  }
}
