object Calculations {
  final val GRAVITATIONAL_CONSTANT = 6.6743015e-11
  /** Old methods for calculating velocities */
  /*
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
    }*/

  case class State(val pos: Vector3D, val v: Vector3D)

  case class Derivative(val dx: Vector3D, val dv: Vector3D)

  //Calculate the average change in position(dx) and velocity(dv = a) using Runge Kutta 4th order
  //This method calculates the position of planets in different times "in the future" and calculates the weighted average based on them
  def calculateAccelerationRK4(bodies: Seq[Body], dt: Double): Seq[(Vector3D, Vector3D)] = {
    val planetInfo = bodies.map(p => (State(p.location, p.velocity), p.mass)) //returns Seq[(State(location, velocity), mass)] = Seq[(State(x0, v0), mass)]
    val k1 = evaluate(planetInfo, 0, bodies.map(a => Derivative(Vector3D(0, 0, 0), Vector3D(0, 0, 0))))
    val k2 = evaluate(planetInfo, dt / 2, k1)
    val k3 = evaluate(planetInfo, dt / 2, k2)
    val k4 = evaluate(planetInfo, dt, k3)

    val dxdt: Seq[Vector3D] = k1.indices.map(i => (k1(i).dx + ((k2(i).dx + k3(i).dx) * 2.0) + k4(i).dx) * (1.0 / 6))
    val dvdt: Seq[Vector3D] = k1.indices.map(i => (k1(i).dv + ((k2(i).dv + k3(i).dv) * 2.0) + k4(i).dv) * (1.0 / 6))
    dxdt zip dvdt
  }

  def evaluate(planets: Seq[(State, Double)], t: Double, ld: Seq[Derivative]): Seq[Derivative] = {
    //Calculate new coordinates based on last derivatives and time (ld = last derivative)
    val states = planets.indices.map(i => State(planets(i)._1.pos + (ld(i).dx * t), planets(i)._1.v + (ld(i).dv * t)))
    //Calculate new Derivative (dx = v, dv = acceleration calculated with new planet coordinates)
    states.indices.map(i => Derivative(
      states(i).v,
      acceleration((states(i).pos, planets(i)._2), (planets.filter(planets(i) != _).map(p => (p._1.pos, p._2))))
    ))
  }

  def acceleration(thisPlanet: (Vector3D, Double), otherPlanets: Seq[(Vector3D, Double)]): Vector3D = {
    var allForces = Vector3D(0, 0, 0)
    for (othetPlanet <- otherPlanets) {
      /** Gravitational force equation: a=(Gc*m1*m2/(r^3*m1))*rc = (Gc*m2/(r^3))*rc , where Gc is graviational constant and rc is vector component for example rx or ry */
      val difInPos = othetPlanet._1 - thisPlanet._1 //(Vector3D, Double) contains (Location, mass)
      val distance = math.sqrt(difInPos.x * difInPos.x + difInPos.y * difInPos.y + difInPos.z * difInPos.z)
      val accelerationWithoutRc = GRAVITATIONAL_CONSTANT * othetPlanet._2 / (distance * distance * distance)
      //accelerationWithoutRComponent * rc for each component x,y and z and add to totalForces
      allForces = allForces + Vector3D(difInPos.x * accelerationWithoutRc, difInPos.y * accelerationWithoutRc, difInPos.z * accelerationWithoutRc)
    }
    allForces
  }
}
