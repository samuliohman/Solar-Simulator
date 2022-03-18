import java.awt.Color
import scala.collection.mutable
import scala.swing.Graphics2D

class SolarSim {
  private var timeStep: Double = 4.0
  val bodies = mutable.Buffer[Body]()
  bodies += new Body("Sun", 10e15, Color.yellow, Vector3D(0, 0, 0), Vector3D(0, 0, 0))
  bodies += new Body("Earth", 10e12, Color.cyan, Vector3D(0, -100, 0), Vector3D(0.5, 0, 0))


  def paint(graphics: Graphics2D, center: (Int, Int)): Unit = {
    for (body <- bodies) {
      //Set the correct color for the object
      graphics.setColor(body.color)

      //Draw ellipse in the location of the body
      val size = math.pow(body.mass, 1.0 / 3) / 2000
      val middle: (Int, Int) = ((center._1 - size / 2.0).toInt, (center._2 - size / 2).toInt)
      graphics.fillOval(body.location.x.toInt + middle._1, body.location.y.toInt + middle._2, size.toInt, size.toInt)
    }
  }

  def update(timeElapsed: Long): Unit = {
    for (body <- bodies) {
      body.applyForce(Calculations.calculateForces(body, bodies.toSeq))
      body.move(timeStep)
    }
  }
}
