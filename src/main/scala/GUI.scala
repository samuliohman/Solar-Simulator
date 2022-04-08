import java.awt.event.ActionListener
import scala.swing.{TextField, _}
import scala.swing.event.ButtonClicked

object GUI extends SimpleSwingApplication {
  override def top: Frame = new MainFrame {
    //Dimensios of of the simulation screen (which doesn't contain left and top bar)
    val simulationWidth = 1024
    val simulationHeight = 576

    //Creates the simulation object
    val simulation = new SolarSim(simulationWidth, simulationHeight)
    var currentPlanetName = ""

    /** Creates the part of the screen where the simulations occurs */
    val simulationScreen = new Panel {
      //Overriding the default method enables us to draw our own graphics.
      override def paintComponent(g: Graphics2D): Unit = {
        simulation.update(System.nanoTime - mostRecentFrame)
        mostRecentFrame = System.nanoTime
        simulation.paint(g, currentPlanetName)
      }
    }

    def updatePlanetInfo(): Unit = {
      currentPlanetName = this.dropdownMenus.head.item
      val newInfoLabels = getPlanetPositionLabels(currentPlanetName, simulation)
      for (i <- newInfoLabels.indices) infoLabels(i).text = newInfoLabels(i).text
      infoLabels.last.text = getPlanetMassLabel(currentPlanetName, simulation).head.text
    }

    //Sets the size of the simulation screen (which doesn't contain left and top bar)
    simulationScreen.minimumSize = new Dimension(simulationWidth, simulationHeight)
    simulationScreen.preferredSize = new Dimension(simulationWidth, simulationHeight)
    simulationScreen.maximumSize = new Dimension(simulationWidth, simulationHeight)

    //Adds the panel that has all the different elements to current window
    val (fullScreen, allButtons, allTextFields, infoLabels, dropdownMenus) = createFullScreen(simulationScreen, simulation)
    contents = fullScreen

    //Add listener to react to button clicks
    allButtons.foreach(listenTo(_))
    reactions += {
      case ButtonClicked(b) => b.text match {
        case "Apply time step" => simulation.changeTimeStep(allTextFields.head.text)
        case "Create new planet" => createNewPlanet(allTextFields.drop(1), dropdownMenus, simulation)
        case "xy -plane" => simulation.changeViewAngle("xy")
        case "xz -plane" => simulation.changeViewAngle("xz")
        case "yz -plane" => simulation.changeViewAngle("yz")
        case "+" => simulation.changeZoom(0.9)
        case "-" => simulation.changeZoom(1.1)
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
      //update planet info labels every 5th frame
      def actionPerformed(e: java.awt.event.ActionEvent) = {
        frames += 1
        if (frames % 5 == 0) updatePlanetInfo()
        if (frames % 100 == 0) {
          println("fps " + fps(lastFPSupdate, 100))
          lastFPSupdate = System.nanoTime
        }
        simulationScreen.repaint()
      }
    }

    // Timer sends ActionEvent to ActionListener as often as possible,
    // when the simulation moves forward and the screen is redrawn.
    // This code therefore allows animation
    var mostRecentFrame = System.nanoTime
    var lastFPSupdate = System.nanoTime
    val timer = new javax.swing.Timer(0, listener)
    timer.start()
  }


  //Gives the fps as double, parameters are nanotime of last update and frames displayed since
  def fps(oldTime: Long, framesSinceLastUpdate: Int) = framesSinceLastUpdate / ((System.nanoTime - oldTime).toDouble / 1000000000)

  //Creates a new planet to the simulation, takes the creation text boxes on the bottom left of the screen as inputs
  def createNewPlanet(creationTextBoxes: Seq[TextField], dropdowns: Seq[ComboBox[String]], simulation: SolarSim) = {
    val planet = simulation.bodies.find(_.name == dropdowns.last.item).getOrElse(throw new Exception("Unknown planet in dropDown"))
    try {
      val asDoubles = creationTextBoxes.take(7).map(_.text.toDouble)
      val location = Vector3D(asDoubles.head, asDoubles(2), asDoubles(4)) + planet.location
      val velocity = Vector3D(asDoubles(1), asDoubles(3), asDoubles(5)) + planet.velocity
      simulation.addBody(creationTextBoxes.last.text, asDoubles.last, location, velocity)

      /** TODO Not working yet */
      //updateDropdows(dropdowns, creationTextBoxes.last.text)
    }
    catch {
      case e: Exception => throw new Exception("No doubles in creation input fields")
    }
  }

  //Adds new planets to the comboboxes

  /** TODO NOT WORKING YET */
  def updateDropdows(dropdowns: Seq[ComboBox[String]], newPlanetName: String) =
    dropdowns.foreach(_.peer.addItem(newPlanetName))

  //Make input double a string with x significant digits and return it in scientific notation
  def customStringFormat(num: Double, significant: Int): String = {
    val sign = if (num < 0) "-" else ""
    val decimals = num.toString.filterNot(char => char == '.' || char == '-')
    val exponent = if (num.toString.contains('E')) num.toString.dropWhile(_ != 'E').drop(1).toInt else num.toInt.toString.length - 1
    s"$sign${decimals.head}.${decimals.slice(1, significant)}e$exponent"
  }

  //Helper method for getting information of currently selected planet
  //Returns sequence of labels that have the information
  def getPlanetPositionLabels(planetName: String, simulation: SolarSim): Seq[Label] = {
    val planet = simulation.bodies.find(_.name == planetName).getOrElse(throw new Exception("Unknown planet in dropDown"))
    Seq(new Label(s"lx ${customStringFormat(planet.location.x, 6)}"), new Label(s"vx ${customStringFormat(planet.velocity.x, 6)}"),
      new Label(s"ly ${customStringFormat(planet.location.y, 6)}"), new Label(s"vy ${customStringFormat(planet.velocity.y, 6)}"),
      new Label(s"lz ${customStringFormat(planet.location.z, 6)}"), new Label(s"vz ${customStringFormat(planet.velocity.z, 6)}"))
  }

  //Returns the planets mass in a label sequence with one instance.
  def getPlanetMassLabel(planetName: String, simulation: SolarSim): Seq[Label] =
    Seq(new Label(s"Mass: ${simulation.bodies.find(_.name == planetName).getOrElse(throw new Exception("Unknown planet in dropDown")).mass}"))


  /** Here we create all the different buttons and text boxes for the GUI --------------------------------------------- */

  /** Creates a panel consisting all the different elements */
  //Returns all the different layouts combined(Boxpanel), all the different buttons used,
  //labels that contain planet information and a dropdown that has current selected planet
  def createFullScreen(simulationScreen: Panel, simulation: SolarSim): (BoxPanel, Seq[Button], Seq[TextField], Seq[Label], Seq[ComboBox[String]]) = {
    val topAndSim = new BoxPanel(Orientation.Vertical)
    val (topBar, topButtons, topTextFields) = createTopBar()
    topAndSim.contents += topBar
    topAndSim.contents += simulationScreen

    val fullPanel = new BoxPanel(Orientation.Horizontal)
    val (leftBar, leftButtons, infoLabels, dropdownMenus, creationBoxes) = createLeftBar(simulation)
    fullPanel.contents += leftBar
    fullPanel.contents += topAndSim

    (fullPanel, (topButtons ++ leftButtons), (topTextFields ++ creationBoxes), infoLabels, dropdownMenus)
  }

  /** Creates the left segment of the screen where there are buttons and text boxes */
  //Returns the layout(Boxpanel), all of the buttons, labels that contain planet information and a dropdown that has current selected planet
  def createLeftBar(simulation: SolarSim): (BoxPanel, Seq[Button], Seq[Label], Seq[ComboBox[String]], Seq[TextField]) = {
    //Creates the upper part of the left bar where information about the planets is presented
    val infoLabelGrid = new GridPanel(2, 1)
    infoLabelGrid.contents += new Label("Here you can see information")
    infoLabelGrid.contents += new Label("about the selected planet")

    var infoDropDown = new ComboBox(simulation.bodies.map(_.name))
    infoDropDown.maximumSize = new Dimension(220, 40)
    val infoGrid = new GridPanel(3, 2)
    val infoLabels: Seq[Label] = getPlanetPositionLabels(infoDropDown.item, simulation)
    infoLabels.foreach(infoGrid.contents += _)

    //Creates the lower part of the left bar where input for creating new object is typed
    val infoText = new Label("Here you can create new planet:")
    val creationGrid = new GridPanel(4, 1)
    val massLabel = getPlanetMassLabel(infoDropDown.item, simulation)
    creationGrid.contents += massLabel.head
    creationGrid.contents += new Label("")
    creationGrid.contents += new Label("")
    creationGrid.contents += infoText

    val createDropDown = new ComboBox[String](simulation.bodies.map(_.name))
    createDropDown.maximumSize = new Dimension(220, 40)

    //Text in the middle
    val creationHeader1 = new Label("Select planet relative to which")
    val creationHeader2 = new Label("inputs are typed (Sun is center)")
    val creationGrid3 = new GridPanel(3, 1)
    creationGrid3.contents += new Label("")
    creationGrid3.contents += creationHeader1
    creationGrid3.contents += creationHeader2

    //Text input fields for creation of new planets
    val creationGrid2 = new GridPanel(4, 2)
    val creationLabels = Seq(new Label("lx"), new Label("vx"), new Label("ly"), new Label("vy"),
      new Label("lz"), new Label("vz"), new Label("mass"), new Label("name"))
    var creationBoxes = Seq[TextField]()

    //Creates the input text boxes for creating new bodies
    for (i <- creationLabels.indices) {
      val flow = new FlowPanel()
      flow.contents += creationLabels(i)
      if (creationLabels(i).text == "name")
        creationBoxes = creationBoxes :+ new TextField("")
      else
        creationBoxes = creationBoxes :+ new TextField("0.0")
      creationBoxes(i).preferredSize = new Dimension(70, 25)
      flow.contents += creationBoxes(i)
      creationGrid2.contents += flow
    }

    val allButtons = Seq(new Button("Create new planet"), new Button("+"), new Button("-"))

    val zoomGrid1 = new GridPanel(1, 2)
    val zoomGrid2 = new GridPanel(2, 1)

    //Creating zoom buttons and labe
    zoomGrid1.contents += allButtons(1)
    zoomGrid1.contents += allButtons(2)
    zoomGrid2.contents += new Label("Zoom")
    zoomGrid2.contents += zoomGrid1

    //combines all of the components
    val verticalPanel = new BoxPanel(Orientation.Vertical)
    val lowGrid = new GridPanel(4, 1)
    lowGrid.contents += allButtons.head
    verticalPanel.contents += infoLabelGrid
    verticalPanel.contents += infoDropDown
    verticalPanel.contents += infoGrid
    verticalPanel.contents += creationGrid
    verticalPanel.contents += creationGrid3
    verticalPanel.contents += createDropDown
    verticalPanel.contents += creationGrid2
    verticalPanel.contents += lowGrid
    verticalPanel.contents += zoomGrid2

    (verticalPanel, allButtons, infoLabels ++ massLabel, Seq(infoDropDown, createDropDown), creationBoxes)
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
