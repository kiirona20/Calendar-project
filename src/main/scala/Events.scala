
import java.io.{BufferedReader, BufferedWriter, FileNotFoundException, FileReader, FileWriter, IOException, PrintWriter, StreamCorruptedException}
import java.time.{LocalDateTime, LocalTime}
import java.util.UUID
import scala.{:+, ::}
import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap
import scala.compiletime.ops.int
import sys.process.*
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object Events {
  // Writes user inputs to a txt file in a correct format.
  def writetoFile(linesToWrite: mutable.LinkedHashMap[String, Seq[String]]):  Unit =
    val fileIn = FileWriter("icalendarformat.txt") // set to true so the writing will append to a file
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

  //resultVcalendar is in ICS format. these indexes will be modified by program.
  //IcalendarFormat converts given Seq[String] to the correct format so it can be written to the file




  //Using linkedHashMap to keep the insertion order
  def iCalderFormatBetter(linestoedit: Seq[Event]): mutable.LinkedHashMap[String, Seq[String]] =
    //randomUUID creates random UUID for the Ics format
    //Date is the key :D
    var empty = mutable.LinkedHashMap[String,Seq[String]]()

    // Checks if the linestoEdit is not empty
    for i <- linestoedit.indices do
      empty =  empty ++ Map(linestoedit(i).startTime -> Seq[String](
       if i == 0 then "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//My Calendar Program//Example Corp//EN\nCALSCALE:GREGORIAN\n\n" else "",
      "BEGIN:VEVENT\nUID:" + linestoedit(i).uid+"\n",
      "DTSTART:" + linestoedit(i).startTime.patch(8,"T",0) + "\n",
      "DTEND:" + linestoedit(i).endTime.patch(8,"T",0) + "\n",
      linestoedit(i).summary.map((j)=>"SUMMARY:" + j + "\n").getOrElse(""),
      linestoedit(i).description.map((j)=>"DESCRIPTION:" + j + "\n").getOrElse(""),
      linestoedit(i).categories.map((j)=>"CATEGORIES:" + j + "\n").getOrElse(""),
      linestoedit(i).trigger.map((j)=>"BEGIN:VALARM\nACTION:AUDIO\nTRIGGER:" + j.patch(8,"T",0) + "\nEND:VALARM\n").getOrElse(""),
      "END:VEVENT\n",
      if i+1 == linestoedit.length then "END:VCALENDAR" else ""
  ))
    empty




  // Reads the whole file and stores the data into a Map.
  // This map can be used to process the data elsewhere.


  //readFile voidaan refraktoroida paremmaksi tekemällä match case tilanne
  def readFileBetter: Map[String,Event] =
    var storedData = ""
    var storedEventsBetter: Map[String,Event] = Map()

    val lineReader = try BufferedReader(FileReader("iCalendarformat.txt"))

      catch
        case e: FileNotFoundException => return storedEventsBetter
        case e: IOException => return storedEventsBetter
    //Testing different required parts of
    var vcalendar = false
    var beginV = false
    var hasUId = false
    var startDateTime = false
    var endDateTime = false
    var endV = false

    var baseEvent = new Event


    try
      var currentLine = lineReader.readLine()
      if currentLine == null then return storedEventsBetter

      currentLine = lineReader.readLine()
      while currentLine != null do
        currentLine = currentLine
        var sisalto = currentLine

        currentLine = currentLine match
          case sisalto if sisalto.startsWith("BEGIN:VEVENT") =>
            beginV = true
            //reset event
            baseEvent = new Event
            lineReader.readLine()

          case sisalto if sisalto.startsWith("UID:") =>
            hasUId = true
            baseEvent.uid = (currentLine.slice(4, 40))
            lineReader.readLine()
          case sisalto if sisalto.startsWith("DTSTART") =>
            startDateTime = true
            baseEvent.startTime = currentLine.slice(8,16) + currentLine.slice(17,23)
            lineReader.readLine()
          case sisalto if sisalto.startsWith("DTEND:") => storedData += "dtend detected"
            endDateTime = true
            baseEvent.endTime = currentLine.slice(6,14) + currentLine.slice(15,21)
            lineReader.readLine()
          case sisalto if sisalto.startsWith("SUMMARY:") => storedData += "summary detected"
            baseEvent.summary = Some(currentLine.drop(8))
            lineReader.readLine()
          case sisalto if sisalto.startsWith("DESCRIPTION:") => storedData += "description detected"
            baseEvent.description = Some(currentLine.drop(12))
            lineReader.readLine()
          case sisalto if sisalto.startsWith("CATEGORIES:") => storedData += "categories detected"
            baseEvent.categories = (Some(currentLine.drop(11)))
            lineReader.readLine()
          case sisalto if sisalto.startsWith("TRIGGER:") => storedData += "trigger detected"
            baseEvent.trigger = (Some(currentLine.slice(8,16) + currentLine.slice(17,23)))
            lineReader.readLine()
          case sisalto if sisalto.startsWith("END:VEVENT") => storedData += "Laita asiat pakettiin"
            endV = true
            lineReader.readLine()
          case _ => lineReader.readLine()
        //If there is all the essential data for creating event the event is stored in a Map
        if beginV && startDateTime && hasUId && endDateTime && endV then
          storedEventsBetter += baseEvent.startTime -> baseEvent
          beginV = false
          hasUId = false
          startDateTime = false
          endDateTime = false
          endV = false




    storedEventsBetter

  //toimii kai
  def addEventBetter(userInput: Event): Unit =
    val existingEvents = readFileBetter
    val updatedEvents = existingEvents.values.toSeq :+ userInput
    val formattedEvents = iCalderFormatBetter(updatedEvents)
    writetoFile(formattedEvents)


  def showEventDetails(userInput: String) = readFileBetter(userInput)

//KEY IS STARTINGDATEANDTIME
//toimii kai
  def editEventBetter(key: String, edit: String, toWhat: Option[String]): Unit =
    val listOfEvents = readFileBetter
    if !listOfEvents.contains(key) then
      println("Event not found!")
      return
    val event = listOfEvents(key)
    event.findAndEdit(edit,toWhat)
    writetoFile(iCalderFormatBetter(listOfEvents.values.toSeq))


// implement method when maptowrite size is
  def deleteEvent(key: String): Unit =
    if readFileBetter.size > 1 then
      val updatedMap = readFileBetter - key
      writetoFile(iCalderFormatBetter(updatedMap.values.toSeq))
    else
      writetoFile(mutable.LinkedHashMap.empty[String, Seq[String]])

  def getEventName(key: String): String =
    readFileBetter(key).summary.getOrElse("Event has no name")

  def getEventEndTime(key: String): String =
    readFileBetter(key).endTime
  def getEventDescription(key: String): String =
    readFileBetter(key).description.getOrElse("Event has no description")
  // 0 = UID 1 = start time 2 = end time 3 = summary  4 = description 5 = categories 6 = alarm
  def getCategorie(key: String) =
    readFileBetter(key).categories.getOrElse("Event has no category")


  def groupedByCategories =
    readFileBetter.groupBy((i)=>i._2.categories).filter((i)=>i._1.isDefined)
  //Filters all the None categories
  def allCategories =
    groupedByCategories.keys
    

  def showEvents =
    if calendarState.appliedFilter.nonEmpty then groupedByCategories.filter((i) => calendarState.appliedFilter.contains(i._1.get)).values.flatMap((i) => i.keys)
    else
      readFileBetter.keys

  private val dateFormat = DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
  private val dateFormat2 = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")
  private val timeFormat = DateTimeFormatter.ofPattern("HHmmss")
  private val dayMonthFormat = DateTimeFormatter.ofPattern("MMdd")

  def getdayOfWeek(date: String): Int =
    LocalDateTime.parse(date,dateFormat).getDayOfWeek.getValue

  def getHour(date: String): Int =
    LocalDateTime.parse(date, dateFormat).getHour

  def getTime(date: String): LocalTime =
    LocalTime.parse(date, dateFormat)

  def getMin(date: String): Int =
    LocalDateTime.parse(date, dateFormat).getMinute

  def convertDate(date: String): LocalDate =
    LocalDateTime.parse(date, dateFormat).toLocalDate

  def convertDateTime(date: String): LocalDateTime =
    LocalDateTime.parse(date, dateFormat)

  def getDateToday = LocalDate.now()
  
  def getDateOnly(date:LocalDate) = date.format(dayMonthFormat)
  
  def convertLocalDateTimeToFormat(date: LocalDateTime) = date.format(dateFormat)
 







}
