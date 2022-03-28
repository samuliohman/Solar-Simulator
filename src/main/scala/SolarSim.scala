import java.awt.{Color, RenderingHints}
import scala.collection.mutable
import scala.swing.Graphics2D

class SolarSim(val simulationWidth: Int, val simulationHeight: Int) {
  private var timeStep: Double = 300.0
  val bodies = mutable.Buffer[Body]()
  bodies += new Body("Sun", 10e15, Color.yellow, Vector3D(0, 0, 0), Vector3D(0, 0, 0))
  bodies += new Body("Earth", 10e12, Color.cyan, Vector3D(0, -100, 0), Vector3D(1.2, 0, 0))
  bodies += new Body("Mars", 10e12, Color.green, Vector3D(0, 150, 0), Vector3D(-1.0, 0, 0))

  //Paints all the bodies in the simulation
  def paint(graphics: Graphics2D, center: (Int, Int)): Unit = {
    // Paint on the background with bright blue
    graphics.setColor(new Color(39, 40, 49))
    graphics.fillRect(0, 0, simulationWidth, simulationHeight)

    // Ask Graphics2D to provide us smoother graphics, i.e., antialising
    graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    for (body <- bodies) {
      //Set the correct color for the object
      graphics.setColor(body.color)

      //Draw ellipse in the location of the body
      val size = math.pow(body.mass, 1.0 / 3) / 2000
      val middle: (Int, Int) = ((center._1 - size / 2.0).toInt, (center._2 - size / 2).toInt)
      graphics.fillOval(body.location.x.toInt + middle._1, body.location.y.toInt + middle._2, size.toInt, size.toInt)
    }
  }

  //Updates the position and velocities of all the bodies in the simulation
  def update(timeElapsed: Long): Unit = {
    val elapsedInSeconds = 1.0 * timeElapsed * math.pow(10, -9)
    for (body <- bodies) {
      body.applyForce(Calculations.calculateForces(body, bodies.toSeq), timeStep * elapsedInSeconds)
      body.move(timeStep * elapsedInSeconds)
    }
  }
}
