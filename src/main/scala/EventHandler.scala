
import scalafx.scene.paint.Color

import java.io.{BufferedReader, BufferedWriter, FileNotFoundException, FileReader, FileWriter, IOException, PrintWriter, StreamCorruptedException}
import java.time.{LocalDateTime, LocalTime}
import java.util.UUID
import scala.{:+, ::}
import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object EventHandler {
  // Writes user inputs to a txt file in a correct format.
  def writetoFile(linesToWrite: mutable.LinkedHashMap[String, Seq[String]]):  Unit =
    val fileIn = FileWriter("eventInfo.ics")
    var lineWriter = BufferedWriter(fileIn)

    try
      //After that we gonna write the linesToWrite part
      linesToWrite.foreach((key)=>key._2.foreach(lineWriter.write(_)))

    catch
      case e: FileNotFoundException => throw e
      case e: IOException => throw e

    finally
      lineWriter.close()
      fileIn.close()

  //Using linkedHashMap to keep the insertion order

  def iCalendarformat(linestoedit: Seq[Event]): mutable.LinkedHashMap[String, Seq[String]] =
    //Initialize LinkedHashMap where start Date is the key
    var iCalendarFormatMap = mutable.LinkedHashMap[String,Seq[String]]()
    //Goes through linestoedit sequence and adds the events in ICS format to the iCalendarFormatMap
    for i <- linestoedit.indices do
      iCalendarFormatMap =  iCalendarFormatMap ++ Map(linestoedit(i).startTime -> Seq[String](
       if i == 0 then "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//My Calendar Program//Example Corp//EN\nCALSCALE:GREGORIAN\n\n" else "",
      "BEGIN:VEVENT\nUID:" + linestoedit(i).uid+"\n",
      "DTSTART:" + linestoedit(i).startTime.patch(8,"T",0) + "\n",
      "DTEND:" + linestoedit(i).endTime.patch(8,"T",0) + "\n",
      linestoedit(i).summary.map((j)=>"SUMMARY:" + j + "\n").getOrElse(""),
      linestoedit(i).description.map((j)=>"DESCRIPTION:" + j + "\n").getOrElse(""),
      linestoedit(i).categories.map((j)=>"CATEGORIES:" + j + "\n").getOrElse(""),
      linestoedit(i).color.map((j)=>"Color:" + j + "\n").getOrElse(""),
      linestoedit(i).trigger.map((j)=>"BEGIN:VALARM\nACTION:AUDIO\nTRIGGER:" + j.patch(8,"T",0) + "\nEND:VALARM\n").getOrElse(""),
      "END:VEVENT\n",
      //adds to the end of the Sequence END:VCALENDAR
      if i+1 == linestoedit.length then "END:VCALENDAR" else ""
  ))
    iCalendarFormatMap




  // Reads the whole file and stores the data into a Map.
  // This map can be used to process the data elsewhere.
  def readFile: Map[String,Event] =

    var storedEvents: Map[String,Event] = Map()
    //tries to find and open iCalendarformat.txt
    val lineReader = try BufferedReader(FileReader("eventInfo.ics"))

      catch
        case e: FileNotFoundException => return storedEvents
        case e: IOException => return storedEvents
    //Initialize the required parts for converting the icalendarformat text to events
    var vcalendar = false
    var beginV = false
    var hasUId = false
    var startDateTime = false
    var endDateTime = false
    var endV = false
    //Initialize event
    var baseEvent = new Event
    //Read the first line
    var currentLine = lineReader.readLine()
    //Goes through the file and detects if are the necessary components are there
    //Slices the unnencessary text that the Event class will not use
    while currentLine != null do
      currentLine = currentLine
      var sisalto = currentLine

      currentLine = currentLine match
        case sisalto if sisalto.startsWith("BEGIN:VEVENT") =>

          beginV = true
          //Starts a new event when Begin:Vevent is detected
          baseEvent = new Event
          lineReader.readLine()

        case sisalto if sisalto.startsWith("UID:") =>
          hasUId = true
          baseEvent.uid = (currentLine.slice(4, 40))
          lineReader.readLine()
        case sisalto if sisalto.startsWith("DTSTART:") =>
          startDateTime = true
          baseEvent.startTime = currentLine.slice(8,16) + currentLine.slice(17,23)
          lineReader.readLine()
        case sisalto if sisalto.startsWith("DTEND:") =>
          endDateTime = true
          baseEvent.endTime = currentLine.slice(6,14) + currentLine.slice(15,21)
          lineReader.readLine()
        case sisalto if sisalto.startsWith("SUMMARY:") =>
          baseEvent.summary = Some(currentLine.drop(8))
          lineReader.readLine()
        case sisalto if sisalto.startsWith("DESCRIPTION:") =>
          baseEvent.description = Some(currentLine.drop(12))
          lineReader.readLine()
        case sisalto if sisalto.startsWith("CATEGORIES:") =>
          baseEvent.categories = (Some(currentLine.drop(11)))
          lineReader.readLine()
        case sisalto if sisalto.startsWith("Color:") =>
          //Drops Color: part and [SFX] part from the file
          baseEvent.color = Some(Color.valueOf(currentLine.drop(11)))
          lineReader.readLine()
        case sisalto if sisalto.startsWith("TRIGGER:") =>
          baseEvent.trigger = (Some(currentLine.slice(8,16) + currentLine.slice(17,23)))
          lineReader.readLine()
        case sisalto if sisalto.startsWith("END:VEVENT") =>
          endV = true
          lineReader.readLine()
        case _ => lineReader.readLine()
        //If there is all the essential data for creating event the event is stored in a Map
      if beginV && startDateTime && hasUId && endDateTime && endV then
        storedEvents += baseEvent.startTime -> baseEvent
        beginV = false
        hasUId = false
        startDateTime = false
        endDateTime = false
        endV = false




    storedEvents
 
  //Adds the event to the txt file
  def addEvent(userInput: Event): Unit =
    val existingEvents = readFile
    val updatedEvents = existingEvents.values.toSeq :+ userInput

    val formattedEvents = iCalendarformat(updatedEvents)
    writetoFile(formattedEvents)


  def showEventDetails(userInput: String): Event = readFile(userInput)



  //Edits and writes the edited event to the txt file
  def editEvent(key: String, edit: String, toWhat: Option[String]): Unit =
    val listOfEvents = readFile
    if !listOfEvents.contains(key) then
      println("Event not found!")
      return
    val event = listOfEvents(key)
    event.findAndEdit(edit,toWhat)
    writetoFile(iCalendarformat(listOfEvents.values.toSeq))


//Deletes Event from a file
  def deleteEvent(key: String): Unit =
    if readFile.size > 1 then
      val updatedMap = readFile - key
      writetoFile(iCalendarformat(updatedMap.values.toSeq))
    else
      writetoFile(mutable.LinkedHashMap.empty[String, Seq[String]])

  def getEventName(key: String): String =
    readFile(key).summary.getOrElse("")

  def getEventEndTime(key: String): String =
    readFile(key).endTime
  def getEventDescription(key: String): String =
    readFile(key).description.getOrElse("")
  def getCategorie(key: String): String =
    readFile(key).categories.getOrElse("")
  def getAlarm(key: String): String =
    readFile(key).trigger.getOrElse("")


  def groupedByCategories: Map[Option[String], Map[String, Event]] =
    readFile.groupBy((i) => i._2.categories).filter((i) => i._1.isDefined)
  //Filters all the None categories


  def allCategories: Iterable[Option[String]] =
    groupedByCategories.keys

  def showAllEvents: Iterable[String] = readFile.keys

  def getEventColor(key: String): Color =
    readFile(key).color.getOrElse(Color.Aqua)

  //Checks the calendarState in case of filters
  //Used in daily and weeklyView to filter the view
  def showEvents: Iterable[String] =
    if calendarState.appliedFilter.nonEmpty then groupedByCategories.filter((i) => calendarState.appliedFilter.contains(i._1.get)).values.flatMap((i) => i.keys)
    else
      showAllEvents

  //Same but for editEventDialog class
  def showEventsEditDialog: Iterable[String] =
    if calendarState.appliedFilter2.nonEmpty then groupedByCategories.filter((i) => calendarState.appliedFilter2.contains(i._1.get)).values.flatMap((i) => i.keys)
    else
      showAllEvents
}
