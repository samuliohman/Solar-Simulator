case class Vector3D(x: Double, y: Double, z: Double) extends fullVector3D

trait fullVector3D {
  def x: Double

  def y: Double

  def z: Double

  def operation(another: fullVector3D, operation: (Double, Double) => Double) = {
    (this, another) match {
      case (Vector3D(x1, y1, z1), Vector3D(x2, y2, z2)) => Vector3D(operation(x1, x2), operation(y1, y2), operation(z1, z2))
      case _ => throw new Exception("Unknown fullVector type")
    }
  }

  def power(pow: Double): Vector3D = operation(Vector3D(pow, pow, pow), (a, b) => math.pow(a, b))

  def +(another: fullVector3D) = operation(another, (a, b) => a + b)

  def -(another: fullVector3D) = operation(another, (a, b) => a - b)

  def *(another: fullVector3D) = operation(another, (a, b) => a * b)

  def *(mult: Double) = operation(Vector3D(mult, mult, mult), (a, b) => a * b)

  def /(another: fullVector3D) = operation(another, (a, b) => a / b)

  def /(mult: Double) = operation(Vector3D(mult, mult, mult), (a, b) => a / b)

  def distance(other: Vector3D):Double = {
    val dif = other - this
    val distance = math.sqrt(dif.x * dif.x + dif.y * dif.y +  dif.z * dif.z)
    distance
  }

  def componentsAsString = s"$x,$y,$z"
}
