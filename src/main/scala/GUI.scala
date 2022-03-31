import java.awt.event.ActionListener
import scala.swing._
import scala.swing.event.ButtonClicked

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
    val simulation = new SolarSim(simulationWidth, simulationHeight)
    //Some helper variables for rendering and calculating
    val center = (((simulationWidth) / 2), ((simulationHeight) / 2))


    /** Creates the part of the screen where the simulations occurs */
    val simulationScreen = new Panel {
      //Overriding the default method enables us to draw our own graphics.
      override def paintComponent(g: Graphics2D): Unit = {
        simulation.update(System.nanoTime - mostRecentFrame)
        mostRecentFrame = System.nanoTime
        simulation.paint(g, center)
      }
    }

    def updatePlaneInfo(): Unit = {
      val newInfoLabels = getPlanetInfo(this.infoDropdown, simulation)
      for (i <- newInfoLabels.indices) infoLabels(i).text = newInfoLabels(i).text
    }

    //Sets the size of the simulation screen (which doesn't contain left and top bar)
    simulationScreen.minimumSize = new Dimension(simulationWidth, simulationHeight)
    simulationScreen.preferredSize = new Dimension(simulationWidth, simulationHeight)
    simulationScreen.maximumSize = new Dimension(simulationWidth, simulationHeight)

    //Adds the panel that has all the different elements to current window
    val (fullScreen, allButtons, allTextFields, infoLabels, infoDropdown) = createFullScreen(simulationScreen, simulation)
    contents = fullScreen

    //Add listener to react to button clicks
    allButtons.foreach(listenTo(_))
    reactions += {
      case ButtonClicked(b) => b.text match {
        case "Apply time step" => simulation.changeTimeStep(allTextFields.head.text)
        case _ =>
      }
    }

    // This event listener and swing timer allow periodic repetitive
    // activity in the event listening thread. The game is light enough
    // to be drawn in the thread without additional buffers or threads.
    var frames = 0
    val listener = new ActionListener() {
      //Methods that doesn't happen every frame are located here
      //Print fps every 100th frame
      //update planet info labels every 10th frame
      def actionPerformed(e: java.awt.event.ActionEvent) = {
        frames += 1
        if(frames % 10 == 0) updatePlaneInfo()
        if (frames % 100 == 0) {
          println("fps " + fps(mostRecentFrame, 1))
          mostRecentFrame = System.nanoTime
        }
        simulationScreen.repaint()
      }
    }

    // Timer sends ActionEvent to ActionListener every 5ms,
    // when the space moves forward and the screen is redrawn.
    // This code therefore allows animation
    var mostRecentFrame = System.nanoTime
    val timer = new javax.swing.Timer(0, listener)
    timer.start()
  }


  //Gives the fps as double, parameters are nanotime of last update and frames displayed since
  def fps(oldTime: Long, framesSinceLastUpdate: Int) = framesSinceLastUpdate / ((System.nanoTime - oldTime).toDouble / 1000000000)

  /** Here we create all the different buttons and text boxes for the GUI --------------------------------------------- */

  /** Creates a panel consisting all the different elements */
  //Returns all the different layouts combined(Boxpanel), all the different buttons used,
  //labels that contain planet information and a dropdown that has current selected planet
  def createFullScreen(simulationScreen: Panel, simulation: SolarSim): (BoxPanel, Seq[Button], Seq[TextField], Seq[Label], ComboBox[String]) = {
    val topAndSim = new BoxPanel(Orientation.Vertical)
    val (topBar, topButtons, topTextFields) = createTopBar()
    topAndSim.contents += topBar
    topAndSim.contents += simulationScreen

    val fullPanel = new BoxPanel(Orientation.Horizontal)
    val (leftBar, leftButtons, infoLabels, infoDropDown) = createLeftBar(simulation)
    fullPanel.contents += leftBar
    fullPanel.contents += topAndSim

    (fullPanel, (topButtons ++ leftButtons), topTextFields, infoLabels, infoDropDown)
  }

  //Helper method for getting information of currently selected planet
  //Returns sequence of labels that have the information
  def getPlanetInfo(infoDropDown: ComboBox[String], simulation: SolarSim): Seq[Label] = {
    val planet = simulation.bodies.find(_.name == infoDropDown.item).getOrElse(throw new Exception("Unknown planet in dropDown"))
    Seq(new Label("lx %.6f".format(planet.location.x)), new Label("vx %.6f".format(planet.velocity.x)),
      new Label("ly %.6f".format(planet.location.y)), new Label("vy %.6f".format(planet.velocity.y)),
      new Label("lz %.6f".format(planet.location.z)), new Label("vz %.6f".format(planet.velocity.z)))
  }

  /** Creates the left segment of the screen where there are buttons and text boxes */
  //Returns the layout(Boxpanel), all of the buttons, labels that contain planet information and a dropdown that has current selected planet
  def createLeftBar(simulation: SolarSim): (BoxPanel, Seq[Button], Seq[Label], ComboBox[String]) = {
    //Creates the upper part of the left bar where information about the planets is presented
    val infoLabelGrid = new GridPanel(2, 1) {
      contents += new Label("Here you can see information")
      contents += new Label("about the selected planet")
    }

    val infoDropDown = new ComboBox(simulation.bodies.map(_.name))
    infoDropDown.maximumSize = new Dimension(440, 40)
    val infoGrid = new GridPanel(3, 2)
    val infoLabels: Seq[Label] = getPlanetInfo(infoDropDown, simulation)
    infoLabels.foreach(infoGrid.contents += _)

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

    (verticalPanel, Seq(button), infoLabels, infoDropDown)
  }


  /** Creates the upper segment of the screen where there are buttons and text boxes */
  //Return the layout as boxpanel, buttons used and the textfield for adjusting time step
  def createTopBar(): (BoxPanel, Seq[Button], Seq[TextField]) = {
    val labelFontSize = 30

    //Creating the buttons and text box concerning camera angles
    val camLabel = new Label("Camera angle")
    camLabel.font = camLabel.font.deriveFont(0, labelFontSize)

    val camButtons = new GridPanel(0, 3)
    var buttons: Seq[Button] = Seq(new Button("xy -plane"), new Button("xz -plane"), new Button("yz -plane"))
    buttons.foreach(camButtons.contents += _)

    val camPanel = new GridPanel(2, 0)
    camPanel.contents += camLabel
    camPanel.contents += camButtons

    //Creating the buttons and text box concerning the adjustment of time step
    val timeStepLabel = new Label("Time step")
    timeStepLabel.peer.setFont(timeStepLabel.peer.getFont.deriveFont(0, labelFontSize))
    val timeStepTextField = new TextField("300.0", 25)
    //Adds time step elements to a panel
    val timeStepElements = new BoxPanel(Orientation.Horizontal)
    timeStepElements.contents += timeStepTextField
    buttons = buttons :+ new Button("Apply time step")
    timeStepElements.contents += buttons(3)
    //Adds time step label and element to a panel
    val timeStepPanel = new GridPanel(2, 0)
    timeStepPanel.contents += timeStepLabel
    timeStepPanel.contents += timeStepElements

    //Adds all the aforementioned elements to one panel
    val horizontalPanel = new BoxPanel(Orientation.Horizontal)
    horizontalPanel.contents += camPanel
    horizontalPanel.contents += timeStepPanel

    (horizontalPanel, buttons, Seq(timeStepTextField))
  }
}
