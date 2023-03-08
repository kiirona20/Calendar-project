import java.io.{
  FileReader,
  FileNotFoundException,
  BufferedReader,
  IOException,
  FileWriter,
  BufferedWriter
}
import scala.util.Random.*
import java.util.UUID

class Events(userInput: String) {

  // Writes user inputs to a txt file.

  def writetoFile: Unit =
    val fileIn = FileWriter("eventInfo.txt")
    var lineWriter = BufferedWriter(fileIn)

    try
      for i <- iCalendarFormat.indices do
        lineWriter.write(iCalendarFormat(i))
        lineWriter.newLine()
    catch
      case e: FileNotFoundException => throw e
      case e: IOException => throw e

    finally
      lineWriter.close()
      fileIn.close()
  //icsFOrmatdontmodify is in ICS format. The program is not going to modify these indexes.
  //resultVcalendar is in ICS format. these indexes will be modified by program.

  def iCalendarFormat: Seq[String] =
    val userInputSplitted = userInput.split(",")
    //randomUUID creates random UUID for the Ics format
    var resultVcalendar = Seq[String]("BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//My Calendar Program//Example Corp//EN\nCALSCALE:GREGORIAN\n\nBEGIN:VEVENT" ,"UID:" + UUID.randomUUID() + "-1234567890@example.com", "DTSTAMP:" + userInputSplitted(0)
      , "DTSTART:" + userInputSplitted(1) + "z", "DTEND:" + userInputSplitted(2) + "z", "SUMMARY:" + userInputSplitted(3), "END:VEVENT\n\nEND:VCALENDAR")
    resultVcalendar














}
