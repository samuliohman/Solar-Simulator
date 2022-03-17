case class Vector3D(x: Double, y: Double, z: Double) extends fullVector3D

trait fullVector3D {
  def x: Double

  def y: Double

  def z: Double

  def operation(another: fullVector3D, operation: (Double, Double) => Double) = {
    this match {
      case Vector3D(x, y, z) => Vector3D(operation(this.x, another.x), operation(this.y, another.y), operation(this.z, another.z))
      case _ => throw new Exception("Unknown fullVector type")
    }
  }

  def +(another: fullVector3D) = operation(another, (a, b) => a + b)

  def -(another: fullVector3D) = operation(another, (a, b) => a - b)

  def *(another: fullVector3D) = operation(another, (a, b) => a * b)

  def /(another: fullVector3D) = operation(another, (a, b) => a / b)

}
