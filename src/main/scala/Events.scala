import java.io.{BufferedReader, BufferedWriter, FileNotFoundException, FileReader, FileWriter, IOException}
import java.util.UUID
import scala.collection.mutable.Map


class Events(userInput: String) {

  // Writes user inputs to a txt file in a correct format.

  def writetoFile: Unit =
    val fileIn = FileWriter("eventInfo.txt",true) // set to true so the writing will append to a file
    var lineWriter = BufferedWriter(fileIn)

    try
      lineWriter.newLine()
      for i <- iCalendarFormat.indices do
        lineWriter.write(iCalendarFormat(i))
        lineWriter.newLine()
    catch
      case e: FileNotFoundException => throw e
      case e: IOException => throw e

    finally
      lineWriter.close()
      fileIn.close()

  //resultVcalendar is in ICS format. these indexes will be modified by program.

  def iCalendarFormat: Seq[String] =
    val userInputSplitted = userInput.split(",")
    //randomUUID creates random UUID for the Ics format
    var resultVcalendar = Seq[String]("BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//My Calendar Program//Example Corp//EN\nCALSCALE:GREGORIAN\n\nBEGIN:VEVENT" ,"UID:" + UUID.randomUUID() + "-1234567890@example.com", "DTSTAMP:" + userInputSplitted(0)
      , "DTSTART:" + userInputSplitted(1) + "z", "DTEND:" + userInputSplitted(2) + "z", "SUMMARY:" + userInputSplitted(3), "END:VEVENT\n\nEND:VCALENDAR")
    resultVcalendar


  // Reads the whole file and stores the data into a Map.
  // This map can be used to process the data elsewhere.
  // UID is used as a key, to be changed later

  def readFile: Map[String,Seq[String]] =
    var storedEvents = Map[String,Seq[String]]()
    var mapKey: String = ""
    val lineReader = try BufferedReader(FileReader("eventInfo.txt"))
    
      catch
        case e: FileNotFoundException => return storedEvents
        case e: IOException => return storedEvents

    var oneline: String = lineReader.readLine()
    while oneline != null do
    
      if oneline.contains("UID:") then
        mapKey = oneline.drop(4).take(36)
        var mapData =  Seq[String]()
        oneline = lineReader.readLine()
        for i <- 0 until(4) do
          if i == 0 then
            mapData = mapData :+ oneline.drop(8)
          if i == 1 then
            mapData = mapData :+ oneline.drop(8)
          if i == 2 then
            mapData = mapData :+ oneline.drop(6)
          if i == 3 then
            mapData = mapData :+ oneline.drop(8)

          oneline = lineReader.readLine()
        storedEvents = storedEvents + (mapKey->mapData)
      oneline = lineReader.readLine()
    storedEvents

  def addEvent = writetoFile
  
  def editEvent = ???
  
  def deleteEvent = ???
















}
