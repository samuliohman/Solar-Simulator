import java.awt.Color

class Body(val name: String, val mass: Double, val radiusReal: Double, val radiusLarge: Double, paramLocation: Vector3D, paramVelocity: Vector3D, val color: Color) {
  private var pLocation = paramLocation
  private var pVelocity = paramVelocity

  def location = pLocation

  def velocity = pVelocity

  //Changes the location of the planet based on velocity and elapsed time
  def move(time: Double) = pLocation = pLocation + (pVelocity * time)

  //Changes the velocity of the planet based on applied force
  def applyForce(force: Vector3D, time: Double) = pVelocity = pVelocity + ((force / mass) * time)

  //Method for applying Runge Kutta average change in location and in velocity
  def applyVelocity(velocityAndAcceleration: (Vector3D, Vector3D), dt: Double, currentBody: Body, simulation: SolarSim) = {
    val otherBodies = simulation.bodies.filterNot(_ == currentBody)
    //Check if new position collides with other planets
    if (otherBodies.exists(other => ((other.radiusReal + currentBody.radiusReal) > other.location.distance(currentBody.location)))) {
      simulation.changeTimeStep("0")
      println("Simulation ended because collision occured")
    } else {
      pLocation = pLocation + velocityAndAcceleration._1 * dt
      pVelocity = pVelocity + velocityAndAcceleration._2 * dt
    }
  }
}
