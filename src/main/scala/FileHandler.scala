import java.awt.Color
import java.io.{BufferedReader, BufferedWriter, FileNotFoundException, FileReader, FileWriter, IOException}

object FileHandler {
  def createPlanetFromString(info: String, simulation: SolarSim) = {
    try {
      val splitted = info.split(",")
      val color = new Color(splitted.last.toInt)
      simulation.addBody(splitted.head, splitted(1).toDouble, splitted(2).toDouble,
        Vector3D(splitted(3).toDouble, splitted(4).toDouble, splitted(5).toDouble),
        Vector3D(splitted(6).toDouble, splitted(7).toDouble, splitted(8).toDouble),
        color)
    }
    catch {
      case e: Exception => throw new Exception("File's data not in correct form")
    }
  }

  def loadPlanetsFromFile(fileName: String, simulation: SolarSim) = {
    val fileReader = try {
      new FileReader(fileName)
    } catch {
      case e: FileNotFoundException => throw new Exception("Planet input file not found")
    }
    val lineReader = new BufferedReader(fileReader)
    try {
      var inputLine = lineReader.readLine()
      while (inputLine != null) {
        createPlanetFromString(inputLine, simulation)
        inputLine = lineReader.readLine()
      }
    } catch {
      case e: IOException => throw new Exception("Error reading the file")
    }
    lineReader.close()
    fileReader.close()
  }

  def saveSimulationToFile(fileName: String, simulation: SolarSim) = {
    def componentsAsString(v: Vector3D) = s"${v.x},${v.y},${v.z}"

    val fileWriter = try {
      new FileWriter(fileName)
    } catch {
      case e: FileNotFoundException => throw new Exception("Error opening save file")
    }
    val lineWriter = new BufferedWriter(fileWriter)
    try {
      for (planet <- simulation.bodies) {
        val otherInfo = s"${planet.name},${planet.mass},${planet.radiusReal}"
        val color = if (planet == simulation.bodies.last) planet.color.getRGB else planet.color.getRGB.toString + "\n"
        lineWriter.write(s"$otherInfo,${componentsAsString(planet.location / 1000.0)},${componentsAsString(planet.velocity / 1000.0)},$color")
      }
    } catch {
      case e: IOException => throw new Exception("Error writing to file")
    }
    lineWriter.close()
    fileWriter.close()
  }
}
