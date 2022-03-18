import java.awt.event.ActionListener
import java.awt.{Color, RenderingHints}
import javax.sound.midi.SysexMessage
import scala.swing._

/** TODO Remove mock bodies */

object GUI extends SimpleSwingApplication {
  val mockBody1 = Seq(1.123, 1.123, 1.123, 1.123, 1.123, 1.123)
  val mockBody2 = Seq(2.456, 2.456, 2.456, 2.456, 2.456, 2.456)
  val mockBody3 = Seq(4.972, 4.972, 4.972, 4.972, 4.972, 4.972)
  val mockbodies = Seq(("Earth", mockBody1), ("Jupiter", mockBody2), ("Venus", mockBody3))


  override def top: Frame = new MainFrame {
    //Dimensios of of the simulation screen (which doesn't contain left and top bar)
    val simulationWidth = 1024
    val simulationHeight = 576

    //Creates the simulation object
    val simulation = new SolarSim

    //Some helper variables for rendering and calculating
    val center = (((simulationWidth) / 2), ((simulationHeight) / 2))
    var lastFrame = System.nanoTime

    /** Creates the part of the screen where the simulations occurs */
    val simulationScreen = new Panel {
      //Overriding the default method enables us to draw our own graphics.
      override def paintComponent(g: Graphics2D) = {
        // Paint on the background with bright blue
        g.setColor(new Color(39, 40, 49))
        g.fillRect(0, 0, simulationWidth, simulationHeight)

        // Ask Graphics2D to provide us smoother graphics, i.e., antialising
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)


        simulation.update(System.nanoTime - lastFrame)
        simulation.paint(g, center)
        lastFrame = System.nanoTime
      }
    }
    //Sets the size of the simulation screen (which doesn't contain left and top bar)
    simulationScreen.minimumSize = new Dimension(simulationWidth, simulationHeight)
    simulationScreen.preferredSize = new Dimension(simulationWidth, simulationHeight)
    simulationScreen.maximumSize = new Dimension(simulationWidth, simulationHeight)

    //Adds the panel that has all the different elements to current window
    contents = createFullScreen(simulationScreen)


    // This event listener and swing timer allow periodic repetitive
    // activity in the event listening thread. The game is light enough
    // to be drawn in the thread without additional buffers or threads.
    var oldTime = System.nanoTime
    var frames = 0
    val listener = new ActionListener() {
      def actionPerformed(e: java.awt.event.ActionEvent) = {
        frames += 1
        if (frames % 300 == 0) {
          println("fps " + fps(oldTime, 300))
          oldTime = System.nanoTime
        }
        simulationScreen.repaint()

      }
    }

    // Timer sends ActionEvent to ActionListener every 5ms,
    // when the space moves forward and the screen is redrawn.
    // This code therefore allows animation
    val timer = new javax.swing.Timer(0, listener)
    timer.start()
  }


  //Gives the fps as double, parameters are nanotime of last update and frames displayed since
  def fps(oldTime: Long, framesSinceLastUpdate: Int) = framesSinceLastUpdate / ((System.nanoTime - oldTime).toDouble / 1000000000)

  /** Here we create all the different buttons and text boxes for the GUI */

  //Creates a panel consisting all the different elements
  def createFullScreen(simulationScreen: Panel): BoxPanel = {
    val topAndSim = new BoxPanel(Orientation.Vertical)
    topAndSim.contents += createTopBar
    topAndSim.contents += simulationScreen

    val fullPanel = new BoxPanel(Orientation.Horizontal)
    fullPanel.contents += createLeftBar
    fullPanel.contents += topAndSim

    fullPanel
  }

  //Creates the left segment of the screen where there are buttons and text boxes
  def createLeftBar = {
    //Creates the upper part of the left bar where information about the planets is presented
    val infoLabelGrid = new GridPanel(2, 1) {
      contents += new Label("Here you can see information")
      contents += new Label("about the selected planet")
    }
    val infoGrid = new GridPanel(3, 2)
    val infoDropDown = new ComboBox(mockbodies.map(_._1))
    infoDropDown.maximumSize = new Dimension(440, 40)
    mockbodies(1)._2.foreach(infoPiece => infoGrid.contents += new Label("v1:  " + infoPiece.toString))

    //Creates the lower part of the left bar where input for creating new object is typed
    val infoText = new Label("Here you can create new planet:")
    val creationGrid = new GridPanel(4, 1)
    creationGrid.contents += new Label("mass: 230234")
    creationGrid.contents += new Label("")
    creationGrid.contents += new Label("")
    creationGrid.contents += infoText

    val createDropDown = new ComboBox(mockbodies.map(_._1))
    createDropDown.maximumSize = new Dimension(440, 40)

    //Text in the middle
    val creationLabel1 = new Label("Select planet relative to which")
    val creationLabel2 = new Label("inputs are typed (Sun is center)")
    val creationGrid3 = new GridPanel(3, 1)
    creationGrid3.contents += new Label("")
    creationGrid3.contents += creationLabel1
    creationGrid3.contents += creationLabel2

    //Text input fields for creation of new planets
    val creationGrid2 = new GridPanel(3, 2)
    for (i <- 0 until 6) {
      val flow = new FlowPanel()
      flow.contents += new Label("v1")
      val inputTextBox = new TextField("302345")
      inputTextBox.preferredSize = new Dimension(70, 25)
      flow.contents += inputTextBox
      creationGrid2.contents += flow
    }
    val button = new Button("Add planet")

    //combines all of the components
    val verticalPanel = new BoxPanel(Orientation.Vertical)
    val lowGrid = new GridPanel(4, 1)
    lowGrid.contents += button
    verticalPanel.contents += infoLabelGrid
    verticalPanel.contents += infoDropDown
    verticalPanel.contents += infoGrid
    verticalPanel.contents += creationGrid
    verticalPanel.contents += creationGrid3
    verticalPanel.contents += createDropDown
    verticalPanel.contents += creationGrid2
    verticalPanel.contents += lowGrid

    verticalPanel
  }


  //Creates the upper segment of the screen where there are buttons and text boxes
  def createTopBar = {
    val labelFontSize = 30

    //Creating the buttons and text box concerning camera angles
    val camLabel = new Label("Camera angle")
    camLabel.font = camLabel.font.deriveFont(0, labelFontSize)
    val camButton1 = new Button("xy -plane")
    val camButton2 = new Button("xz -plane")
    val camButton3 = new Button("yz -plane")

    val camButtons = new GridPanel(0, 3)
    camButtons.contents += camButton1
    camButtons.contents += camButton2
    camButtons.contents += camButton3

    val camPanel = new GridPanel(2, 0)
    camPanel.contents += camLabel
    camPanel.contents += camButtons

    //Creating the buttons and text box concerning the adjustment of time step
    val timeStepLabel = new Label("Time step")
    timeStepLabel.peer.setFont(timeStepLabel.peer.getFont.deriveFont(0, labelFontSize))
    val timeStepTextField = new TextField("Please input a double", 25)
    val timeStepButton = new Button("Apply time step")

    val timeStepElements = new BoxPanel(Orientation.Horizontal)
    timeStepElements.contents += timeStepTextField
    timeStepElements.contents += timeStepButton

    val timeStepPanel = new GridPanel(2, 0)
    timeStepPanel.contents += timeStepLabel
    timeStepPanel.contents += timeStepElements

    //Adds all the aforementioned elements to one panel
    val horizontalPanel = new BoxPanel(Orientation.Horizontal)
    horizontalPanel.contents += camPanel
    horizontalPanel.contents += timeStepPanel

    horizontalPanel
  }
}
