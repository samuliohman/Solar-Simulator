import java.awt.Color
import java.io.{BufferedReader, FileNotFoundException, FileReader, IOException}

object FileHandler {
  def createPlanetFromString(info: String, simulation: SolarSim) = {
    val splitted = info.split(",")
    println(splitted(1).toDouble +" "+splitted(2).toDouble +" "+splitted(3).toDouble +" "+splitted(4).toDouble)
    val color = splitted.last match {
      case "yellow" => Color.yellow
      case "green" => Color.green
      case "orange" => Color.orange
      case "lightGray" => Color.lightGray
      case "brown" => new Color(153,102,0)
      case "lightBlue" => new Color(51,204,255)
      case "gold" => new Color(255,204,51)
      case _ => Color.white
    }

    try{
    simulation.addBody(splitted.head, splitted(1).toDouble, splitted(2).toDouble,
      Vector3D(splitted(3).toDouble, splitted(4).toDouble, splitted(5).toDouble),
      Vector3D(splitted(6).toDouble, splitted(7).toDouble, splitted(8).toDouble),
      color)
    }
    catch{
      case e: Exception => throw new Exception("File's data not in correct form")
    }
  }

  def loadPlanetsFromFile(fileName: String, simulation: SolarSim) = {
    val fileReader = try{
      new FileReader(fileName)
    }catch{
      case e: FileNotFoundException => throw new Exception("Planet input file not found")
    }
    val lineReader = new BufferedReader(fileReader)
    try{
      var inputLine = lineReader.readLine()
      while(inputLine != null){
        createPlanetFromString(inputLine, simulation)
        inputLine = lineReader.readLine()
      }
    }catch{
      case e: IOException => throw new Exception("Error reading the file")
    }
    fileReader.close()
    lineReader.close()
  }
  def saveSimulationToFile = ???
}
