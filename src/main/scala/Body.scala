import java.awt.Color

class Body(val name: String, val mass: Double, val radiusReal: Double, val radiusLarge: Double, paramLocation: Vector3D, paramVelocity: Vector3D, val color: Color) {
  private var pLocation = paramLocation//BigDecVector3D(BigDecimal.valueOf(paramLocation.x),BigDecimal.valueOf(paramLocation.y),BigDecimal.valueOf(paramLocation.z))
  private var pVelocity = paramVelocity

  def location = pLocation

  def velocity = pVelocity

  def copy = new Body(name, mass, radiusReal, radiusLarge, pLocation, pVelocity, color)

  //Method for applying Runge Kutta average change in location and in velocity
  def applyVelocityAndAcceleration(velocityAndAcceleration: (Vector3D, Vector3D), dt: Double, currentBody: Body, simulation: SolarSim) = {
    val otherBodies = simulation.bodies.diff(Seq(currentBody))
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
