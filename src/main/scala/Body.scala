class Body(val name: String, val mass: Double, paramLocation: Vector3D, paramVelocity: Vector3D) {
  private var pLocation = paramLocation
  private var pVelocity = paramVelocity

  def location = pLocation

  def velocity = pVelocity

  def move() = pLocation = pLocation + pVelocity

  def applyForce(force: Vector3D) = pVelocity = pVelocity + force

}
