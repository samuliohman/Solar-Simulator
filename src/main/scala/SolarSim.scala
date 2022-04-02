import java.awt.{Color, RenderingHints}
import scala.collection.mutable
import scala.swing.Graphics2D

class SolarSim(val simulationWidth: Int, val simulationHeight: Int) {
  private var timeStep: Double = 3 * 86400 // 86400 seconds in one day, so default speed of simulation is x days per second
  val bodies = mutable.Buffer[Body]()
  bodies += new Body("Sun", 1.99847e30, Color.yellow, Vector3D(0, 0, 0), Vector3D(0, 0, 0))
  bodies += new Body("Earth", 5.9722e24, Color.green, Vector3D(0, 150e9, 0), Vector3D(30000, 0, 0))
  bodies += new Body("Satellite", 5e5, Color.cyan, Vector3D(30e9, 50e9, 0), Vector3D(30000, -50000, 0))


  //Some helper attributes for drawing planets in correct locations
  //Set the width of simulation screen in kilometers and calculate simulation height from aspect ratio and width
  val widthOfSimulation: Double = 600e9
  val heightOfSimulation: Double = simulationHeight.toDouble / simulationWidth * widthOfSimulation

  //Check if input was a double between 1500 and 0. Then apply new timeStep
  def changeTimeStep(newStep: String) =
    if (newStep.toDoubleOption.nonEmpty) timeStep = newStep.toDouble * 86400 else throw new Exception("Time step must be a double")

  //Function that takes coordinates in meters as paremeter and the planet size and return coordinates in pixels
  def coordinatesToPixels(location: Vector3D, size: Double): Vector3D = {
    val locationCentered = location + Vector3D((widthOfSimulation / 2), heightOfSimulation / 2, 0)
    val locationNormalized = locationCentered / Vector3D(widthOfSimulation, heightOfSimulation, 0)
    val locationInPixels = locationNormalized * Vector3D(simulationWidth, simulationHeight, 0)
    locationInPixels - Vector3D(size / 2, size / 2, 0)
  }

  //Paints all the bodies in the simulation
  def paint(graphics: Graphics2D): Unit = {
    // Paint on the background with bright blue
    graphics.setColor(new Color(39, 40, 49))
    graphics.fillRect(0, 0, simulationWidth, simulationHeight)

    // Ask Graphics2D to provide us smoother graphics, i.e., antialising
    graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    for (body <- bodies) {
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
