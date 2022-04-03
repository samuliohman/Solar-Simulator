import java.awt.{Color, RenderingHints}
import scala.collection.mutable
import scala.swing.Graphics2D

class SolarSim(val simulationWidthInPixels: Int, val simulationHeightInPixels: Int) {
  private var timeStep: Double = 3 * 86400 // 86400 seconds in one day, so default speed of simulation is x days per second
  private var viewAngle = "xy" //Set default view angle
  val bodies = mutable.Buffer[Body]()
  bodies += new Body("Sun", 1.99847e30, Color.yellow, Vector3D(0, 0, 0), Vector3D(0, 0, 0))
  bodies += new Body("Earth", 5.9722e24, Color.green, Vector3D(0, 150e9, 0), Vector3D(30000, 0, 0))
  bodies += new Body("Satellite", 5e5, Color.cyan, Vector3D(30e9, 50e9, 0), Vector3D(30000, -50000, 0))
  bodies += new Body("Satellite2", 5e5, Color.red, Vector3D(30e9, 50e9, 10e9), Vector3D(30000, -40000, 15000))

  //Some helper attributes for drawing planets in correct locations
  //Set the width of simulation screen in kilometers and calculate simulation height from aspect ratio and width
  val widthOfSimulation = 600e9
  val heightOfSimulation = simulationHeightInPixels.toDouble / simulationWidthInPixels * widthOfSimulation

  //Function for updating camera angle
  def changeViewAngle(newAngle: String) = viewAngle = newAngle

  //Check that new time step is a double and update the time step
  //time step scale: if time step is one, it means second in real life means a day in simulation
  def changeTimeStep(newStep: String) =
    if (newStep.toDoubleOption.nonEmpty) timeStep = newStep.toDouble * 86400 else throw new Exception("Time step must be a double")

  //Function that takes coordinates in meters as paremeter and the planet size and return coordinates in pixels
  def coordinatesToPixels(location: Vector3D, size: Double): Vector3D = {
    //Set the correct point of view according to viewAngle
    var locationCentered = viewAngle match {
      case "xy" => Vector3D(location.x + widthOfSimulation / 2, location.y + heightOfSimulation / 2, 0)
      case "xz" => Vector3D(location.x + widthOfSimulation / 2, location.z + heightOfSimulation / 2, 0)
      case "yz" => Vector3D(location.y + widthOfSimulation / 2, location.z + heightOfSimulation / 2, 0)
      case _ => {
        Vector3D(0, 0, 0)
        throw new Exception("Unkown view angle")
      }
    }
    val locationNormalized = locationCentered / Vector3D(widthOfSimulation, heightOfSimulation, 0)
    val locationInPixels = locationNormalized * Vector3D(simulationWidthInPixels, simulationHeightInPixels, 0)
    locationInPixels - Vector3D(size / 2, size / 2, 0)
  }

  //Paints all the bodies in the simulation
  def paint(graphics: Graphics2D): Unit = {
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
      val size = math.max(math.pow(body.mass, 1.0 / 5) / 10000, 5)
      val coordinatesInPixels = coordinatesToPixels(body.location, size)
      graphics.fillOval(coordinatesInPixels.x.toInt, coordinatesInPixels.y.toInt, size.toInt, size.toInt)
    }
  }

  //Updates the position and velocities of all the bodies in the simulation
  def update(timeElapsed: Long): Unit = {
    val elapsedInSeconds = 1.0 * timeElapsed / 1e9
    for (body <- bodies) {
      body.applyForce(Calculations.calculateForces(body, bodies.toSeq), timeStep * elapsedInSeconds)
      body.move(timeStep * elapsedInSeconds)
    }
  }
}
