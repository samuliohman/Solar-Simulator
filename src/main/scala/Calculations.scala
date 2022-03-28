object Calculations {
  def calculateForce(b1: Body, b2: Body): Vector3D = {
    val differenceInPosition = (b2.location - b1.location)
    val distanceSquared = differenceInPosition.power(2)
    val distance = math.sqrt(distanceSquared.x +distanceSquared.y + distanceSquared.z)
    differenceInPosition * (0.4*math.pow(10, -18) * b1.mass*b2.mass/distance)
  }

  def calculateForces(current: Body, bodies: Seq[Body]): Vector3D = {
    val otherBodies = bodies.filterNot( _ == current )
    var sumOfForces = Vector3D(0,0,0)
    for(body <- otherBodies){
      sumOfForces += calculateForce(current, body)
    }
    sumOfForces
  }
}
