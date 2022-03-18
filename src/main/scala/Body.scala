import java.awt.Color

class Body(val name: String, val mass: Double, val color: Color, paramLocation: Vector3D, paramVelocity: Vector3D) {
  private var pLocation = paramLocation
  private var pVelocity = paramVelocity

  def location = pLocation

  def velocity = pVelocity

  def move(timeStep: Double) = pLocation = pLocation + (pVelocity * Vector3D(timeStep, timeStep, timeStep))

  def applyForce(force: Vector3D) = pVelocity = pVelocity + (force / mass)
}
