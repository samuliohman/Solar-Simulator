import java.awt.{Color, RenderingHints}
import scala.collection.mutable
import scala.swing.Graphics2D

class SolarSim(val simulationWidthInPixels: Int, val simulationHeightInPixels: Int) {
  private var timeStep: Double = 1 * 86400 // 86400 seconds in one day, so default speed of simulation is x days per second
  private var timeRun = 0.0
  private var viewAngle = "xy" //Set default view angle
  private var zoom = 1.0
  val defaultSimulationWidth = 680e9
  val bodies = mutable.Buffer[Body]()

  //Location for saving file and the location from which the simulation is loaded
  val saveLocation = "src/main/scala/SaveFiles/SolarSimSaved.txt"
  FileHandler.loadPlanetsFromFile("src/main/scala/SaveFiles/planetInfoBasic.txt", this)

  //Function for updating camera angle
  def changeViewAngle(newAngle: String) = viewAngle = newAngle

  //Get time total runtime of the simulation in days
  def getTimeRun = timeRun / 86400

  //Check that new time step is a double and update the time step
  //time step scale: if time step is one, it means second in real life means a day in simulation
  def changeTimeStep(newStep: String) =
    if (newStep.toDoubleOption.nonEmpty) timeStep = newStep.toDouble * 86400 else throw new Exception("Time step must be a double")

  //Change current zoom. Coefficient > 1 => zoom out and coefficient < 1 => zoom in
  def changeZoom(zoomCoefficient: Double) = this.zoom = this.zoom * zoomCoefficient

  //Add new body to the simulation
  def addBody(name: String, mass: Double, radius: Double, location: Vector3D, velocity: Vector3D, color: Color = Color.white) =
    bodies += new Body(name, mass, radius, math.pow(mass / 4000 * 3 / (4 * math.Pi), 1.0 / 3), location * 1000, velocity * 1000, color)

  //Function that takes coordinates in meters as paremeter and the planet size and return coordinates in pixels
  def coordinatesToPixels(location: Vector3D, size: Double, relativeTo: Vector3D): Vector3D = {
    //Set the width of simulation screen in meters and calculate simulation height from aspect ratio and width
    val widthOfSimulation = defaultSimulationWidth * zoom
    val heightOfSimulation = simulationHeightInPixels.toDouble / simulationWidthInPixels * widthOfSimulation
    //Set the correct point of view according to viewAngle
    val locationCentered = viewAngle match {
      case "xy" => Vector3D(location.x + widthOfSimulation / 2, location.y + heightOfSimulation / 2, 0) - Vector3D(relativeTo.x, relativeTo.y, 0)
      case "xz" => Vector3D(location.x + widthOfSimulation / 2, location.z + heightOfSimulation / 2, 0) - Vector3D(relativeTo.x, relativeTo.z, 0)
      case "yz" => Vector3D(location.y + widthOfSimulation / 2, location.z + heightOfSimulation / 2, 0) - Vector3D(relativeTo.y, relativeTo.z, 0)
      case _ => Vector3D(0, 0, 0)
    }
    val locationNormalized = locationCentered / Vector3D(widthOfSimulation, heightOfSimulation, 1)
    val locationInPixels = locationNormalized * Vector3D(simulationWidthInPixels, simulationHeightInPixels, 0)
    locationInPixels - Vector3D(size / 2, size / 2, 0)
  }

  //Paints all the bodies in the simulation
  def paint(graphics: Graphics2D, planetName: String): Unit = {
    //Get currently selected planet relative to which all other planets are rendered
    val selectedPlanet = if (bodies.map(_.name).contains(planetName)) bodies.find(_.name == planetName).get.location else Vector3D(0, 0, 0)

    // Paint on the background with bright blue
    graphics.setColor(new Color(39, 40, 49))
    graphics.fillRect(0, 0, simulationWidthInPixels, simulationHeightInPixels)

    // Ask Graphics2D to provide us smoother graphics, i.e., antialising
    graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    //Sort all the bodies according to viewAngle so that a correct body is drawn on top of the body which is "underneath"
    val bodiesSorted = bodies.sortBy(body => viewAngle match {
      case "xy" => body.location.z
      case "xz" => body.location.y
      case "yz" => body.location.x
      case _ => body.mass
    })
    for (body <- bodiesSorted) {
      //Set the correct color for the object
      graphics.setColor(body.color)

      //Draw ellipse in the location of the body
      val size = math.max(body.radiusLarge / 15000000 / math.pow(zoom, 1.0 / 1.3), 5)
      val coordinatesInPixels = coordinatesToPixels(body.location, size, selectedPlanet)
      graphics.fillOval(coordinatesInPixels.x.toInt, coordinatesInPixels.y.toInt, size.toInt, size.toInt)
    }
  }

  //Updates the position and velocities of all the bodies in the simulation
  def update(timeElapsed: Long): Unit = {
    val elapsedInSeconds = 1.0 * timeElapsed / 1e9
    val dt = timeStep * elapsedInSeconds
    timeRun += dt
    val derivatives = Calculations.calculateAccelerationRK4(bodies.toSeq, dt)
    bodies.indices.foreach(i => bodies(i).applyVelocity(derivatives(i), dt, bodies(i), this))
  }
}
