import java.awt.event.ActionListener
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.util.control.Breaks._

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
      override def paintComponent(g: Graphics2D): Unit = simulation.paint(g, currentPlanetName)
    }
    //Sets the size of the simulation screen (which doesn't contain left and top bar)
    simulationScreen.minimumSize = new Dimension(simulationWidth, simulationHeight)
    simulationScreen.preferredSize = new Dimension(simulationWidth, simulationHeight)
    simulationScreen.maximumSize = new Dimension(simulationWidth, simulationHeight)

    //Updates the text labels on the left that display information about the current planet
    def updatePlanetInfo(): Unit = {
      currentPlanetName = this.dropdownMenus.head.item
      val newInfoLabels = getPlanetPositionLabels(currentPlanetName, simulation)
      for (i <- newInfoLabels.indices) infoLabels(i).text = newInfoLabels(i)
      val mass = getPlanetMassLabel(currentPlanetName, simulation)
      infoLabels.dropRight(1).last.text = mass.text
      infoLabels.last.text = "Time: %.2f days".format(simulation.getTimeRun)
    }

    //Update the fps labels on the left side of the screen
    def updateFpsInfo(): Unit = {
      val fpsInfo = getFPSInfo(ScreenThreadUpdates, SimThreadUpdates, System.nanoTime - lastFPSupdate)
      ScreenThreadUpdates = 0
      SimThreadUpdates = 0
      lastFPSupdate = System.nanoTime
      infoLabels.dropRight(3).last.text = fpsInfo._1
      infoLabels.dropRight(2).last.text = fpsInfo._2
    }

    //Adds the panel that has all the different elements to current window
    val (fullScreen, allButtons, allTextFields, infoLabels, dropdownMenus) = createFullScreen(simulationScreen, simulation)
    contents = fullScreen

    //Add listener to react to button clicks
    allButtons.foreach(listenTo(_))
    reactions += {
      case ButtonClicked(b) => b.text match {
        case "Apply step size" => simulation.changeTimeStepSize(allTextFields.head.text)
        case "Apply step length" => simulation.changeTimeStepLength(allTextFields(1).text)
        case "Create new planet" => newBodyCreated = true
        case "save" => FileHandler.saveSimulationToFile(simulation.saveLocation, simulation)
        case "xy -plane" => simulation.changeViewAngle("xy")
        case "xz -plane" => simulation.changeViewAngle("xz")
        case "yz -plane" => simulation.changeViewAngle("yz")
        case "+" => simulation.changeZoom(0.7)
        case "-" => simulation.changeZoom(1.3)
        case _ =>
      }
    }

    //This event listener and swing timer allow periodic repetitive activity in the event listening thread. Used for drawing planets and updating GUI
    val listener = new ActionListener() {
      def actionPerformed(e: java.awt.event.ActionEvent) = {
        frames += 1
        ScreenThreadUpdates += 1

        //repainting the simulation (planets are updated every frame, planet information on the side every 5th frame and fps information every 100th frame)
        simulationScreen.repaint()
        if (frames % 5 == 0) updatePlanetInfo()
        if (frames % 300 == 0) updateFpsInfo()
      }
    }

    //Added the simulation to its own thread so its able to run as fast as possible. This way simulation thread doesn't have to wait for painting and updating screen
    val simulationThread = new Thread {
      override def run() = {
        while (true) {
          SimThreadUpdates += 1
          //Creating new object in simulation thread to avoid errors in using same object in different threads
          if (newBodyCreated) {
            createNewPlanet(allTextFields.drop(2), dropdownMenus, simulation)
            newBodyCreated = false
          }

          //Updating simulation
          val elapsedTime = System.nanoTime - lastFrame
          val sleepAmount = (simulation.getTimeStepLength - (elapsedTime / 1e6)).toInt
          if (sleepAmount > 0) Thread.sleep(sleepAmount)
          lastFrame = System.nanoTime
          simulation.update()
        }
      }
    }
    simulationThread.start()

    //Variables for calculating the frames per seconds that different threads have
    var ScreenThreadUpdates = 0
    var SimThreadUpdates = 0
    var lastFPSupdate = System.nanoTime

    //Variables for tracting time between steps and for creating new bodies
    var lastFrame = System.nanoTime
    var frames = 0
    var newBodyCreated = false

    // Timer sends ActionEvent to ActionListener as often as possible, this code therefore allows animation
    val timer = new javax.swing.Timer(0, listener)
    timer.start()
  }

  //Returns the different threads fps's.
  def getFPSInfo(frames1: Int, frames2: Int, elapsed: Long): (String, String) = {
    val fps1 = frames1 / (elapsed / 1e9)
    val fps2 = frames2 / (elapsed / 1e9)
    ("fps(GUI thread) %.2f".format(fps1), "fps(Sim thread) %.2f".format(fps2))
  }

  //Creates a new planet to the simulation, takes the creation text boxes on the bottom left of the screen as parameters
  def createNewPlanet(creationTextBoxes: Seq[TextField], dropdowns: Seq[ComboBox[String]], simulation: SolarSim) = breakable {
    val planet = simulation.bodies.find(_.name == dropdowns.last.item).getOrElse(throw new Exception("Unknown planet in dropDown"))
    if (creationTextBoxes.last.text == "") {
      println("Planet's name is empty")
      break
    } else if (creationTextBoxes.take(7).map(_.text.toDoubleOption).exists(_.isEmpty)) {
      println("No doubles in creation input fields")
      break
    } else {
      val asDoubles = creationTextBoxes.take(7).map(_.text.toDouble)
      if (asDoubles.last <= 0) {
        println("Mass must be over 0")
        break
      }
      val location = Vector3D(asDoubles.head, asDoubles(2), asDoubles(4)) + (planet.location / 1000)
      val velocity = Vector3D(asDoubles(1), asDoubles(3), asDoubles(5)) + (planet.velocity / 1000)
      simulation.addBody(creationTextBoxes.last.text, asDoubles.last, 0, location, velocity)
    }
  }

  //Make input double a string with x significant digits and return it in scientific notation
  def customStringFormat(num: Double, significant: Int): String = {
    val sign = if (num < 0) "-" else ""
    val decimals = num.toString.filterNot(char => char == '.' || char == '-')
    val exponent = if (num.toString.contains('E')) num.toString.dropWhile(_ != 'E').drop(1).toInt else num.toInt.toString.length - 1
    s"$sign${decimals.head}.${decimals.slice(1, significant)}e$exponent"
  }

  //Helper method for getting information of currently selected planet
  def getPlanetPositionLabels(planetName: String, simulation: SolarSim): Seq[String] = {
    val planet = simulation.bodies.find(_.name == planetName).getOrElse(throw new Exception("Unknown planet in dropDown"))
    Seq(s"lx ${customStringFormat(planet.location.x, 6)}", s"vx ${customStringFormat(planet.velocity.x, 6)}",
      s"ly ${customStringFormat(planet.location.y, 6)}", s"vy ${customStringFormat(planet.velocity.y, 6)}",
      s"lz ${customStringFormat(planet.location.z, 6)}", s"vz ${customStringFormat(planet.velocity.z, 6)}")
  }

  //Returns the planets mass in an label.
  def getPlanetMassLabel(planetName: String, simulation: SolarSim): Label =
    new Label(s"Mass: ${simulation.bodies.find(_.name == planetName).getOrElse(throw new Exception("Unknown planet in dropDown")).mass}")


  /** Here we create all the different buttons and text boxes for the GUI --------------------------------------------- */

  /** Creates a panel consisting all the different elements */
  //Returns all the different layouts combined(Boxpanel), all the different buttons used,
  //labels that contain planet information and dropdowns that have planet selected
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
    val infoLabels: Seq[Label] = Seq.fill(6)(new Label(""))
    infoLabels.indices.foreach(i => infoLabels(i).text = getPlanetPositionLabels(infoDropDown.item, simulation)(i))
    infoLabels.foreach(infoGrid.contents += _)

    //FPS labels
    val fpsLabels = Seq(new Label("fps(ScreenThread) "), new Label("fps(Simulation) "))
    val fpsGrid = new GridPanel(2, 1)
    fpsGrid.contents += fpsLabels.head
    fpsGrid.contents += fpsLabels.last

    //Creates the lower part of the left bar where input for creating new object is typed
    val infoText = new Label("Here you can create new planet:")
    val creationGrid = new GridPanel(5, 1)
    val massAndTimeLabels = Seq(getPlanetMassLabel(infoDropDown.item, simulation), new Label("Time: %.2f days".format(simulation.getTimeRun)))
    creationGrid.contents += massAndTimeLabels.head
    creationGrid.contents += massAndTimeLabels.last
    creationGrid.contents += fpsGrid
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

    val allButtons = Seq(new Button("Create new planet"), new Button("+"), new Button("-"), new Button("save"))
    val zoomGrid1 = new GridPanel(1, 2)
    val bottomLeft = new GridPanel(4, 1)

    //Creating zoom buttons and labe
    zoomGrid1.contents += allButtons(1)
    zoomGrid1.contents += allButtons(2)
    bottomLeft.contents += new Label("Zoom")
    bottomLeft.contents += zoomGrid1
    bottomLeft.contents += new Label("Save to " + simulation.saveLocation)
    bottomLeft.contents += allButtons(3)

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
    verticalPanel.contents += bottomLeft

    (verticalPanel, allButtons, infoLabels ++ fpsLabels ++ massAndTimeLabels, Seq(infoDropDown, createDropDown), creationBoxes)
  }


  /** Creates the upper segment of the screen where there are buttons and text boxes */
  //Return the layout as boxpanel, buttons used and the textfield for adjusting time step
  def createTopBar(): (BoxPanel, Seq[Button], Seq[TextField]) = {
    val labelFontSize = 25

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
    val timeStepLabel = new Label("Simulation time passed per step (seconds)")
    val timeStepTextField = new TextField("100.0")
    buttons = buttons :+ new Button("Apply step size")
    val timeStepLabel2 = new Label("Time step length in ms(0 for max speed at around 5000 fps)")
    val timeStepTextField2 = new TextField("10.0")
    buttons = buttons :+ new Button("Apply step length")

    //Adds time step elements to a panel
    val timeStepElements = new BoxPanel(Orientation.Horizontal)
    timeStepElements.contents += timeStepTextField
    timeStepElements.contents += buttons(3)
    val timeStepElements2 = new BoxPanel(Orientation.Horizontal)
    timeStepElements2.contents += timeStepTextField2
    timeStepElements2.contents += buttons(4)

    //Adds time step label and element to a panel
    val timeStepPanel = new GridPanel(2, 2)
    timeStepPanel.contents += timeStepLabel
    timeStepPanel.contents += timeStepLabel2
    timeStepPanel.contents += timeStepElements
    timeStepPanel.contents += timeStepElements2

    //Adds all the aforementioned elements to one panel
    val horizontalPanel = new BoxPanel(Orientation.Horizontal)
    horizontalPanel.contents += camPanel
    horizontalPanel.contents += timeStepPanel

    (horizontalPanel, buttons, Seq(timeStepTextField, timeStepTextField2))
  }
}
