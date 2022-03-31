import java.awt.Color

class Body(val name: String, val mass: Double, val color: Color, paramLocation: Vector3D, paramVelocity: Vector3D) {
  private var pLocation = paramLocation
  private var pVelocity = paramVelocity

  def location = pLocation

  def velocity = pVelocity

  //Changes the location of the planet based on velocity and elapsed time
  def move(time: Double) = pLocation = pLocation + (pVelocity * Vector3D(time, time, time))

  //Changes the velocity of the planet based on applied force
  def applyForce(force: Vector3D, time: Double) = pVelocity = pVelocity + ((force / mass) * time)
}
