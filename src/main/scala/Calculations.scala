object Calculations {
  final val GRAVITATIONAL_CONSTANT = 6.6743015e-11

  def calculateForce(b1: Body, b2: Body): Vector3D = {
    /** Gravitational force equation: F=(Gc*m1*m2/r^3)*rc, where Gc is graviational constant and rc is vector component for example rx or ry */
    val difInPosition = (b2.location - b1.location) // =Vector3D(dx, dy, dz)
    val OneDimensionalDif = // =sqrt(dx.abs^2 + dy.abs^2 + dz.abs^2)
      math.sqrt(difInPosition.x * difInPosition.x + difInPosition.y * difInPosition.y + difInPosition.z * difInPosition.z)
    val distanceCubed = OneDimensionalDif * OneDimensionalDif * OneDimensionalDif // =r^3

    val forceWithoutRComponent = GRAVITATIONAL_CONSTANT * b1.mass * b2.mass / distanceCubed //=Gc * m1 * m2 / r^3
    //forceWithoutRComponent * rc for each component x,y and z
    Vector3D(difInPosition.x * forceWithoutRComponent, difInPosition.y * forceWithoutRComponent, difInPosition.z * forceWithoutRComponent)
  }

  def calculateForces(current: Body, bodies: Seq[Body]): Vector3D = {
    val otherBodies = bodies.filterNot(_ == current)
    var sumOfForces = Vector3D(0, 0, 0)
    for (body <- otherBodies) {
      sumOfForces += calculateForce(current, body)
    }
    sumOfForces
  }
}
