

import java.io.{BufferedReader, BufferedWriter, FileNotFoundException, FileReader, FileWriter, IOException, PrintWriter}
import java.time.LocalDateTime
import java.util.UUID
import scala.:+
import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap
import scala.compiletime.ops.int
//import scala.sys.process.processInternal.File
import sys.process.*
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object Events {

  // Writes user inputs to a txt file in a correct format.


  def writetoFile(linesToWrite: Map[String, Seq[String]]):  Unit =
    val fileIn = FileWriter("eventInfo.txt") // set to true so the writing will append to a file
    var lineWriter = BufferedWriter(fileIn)

    try
      lineWriter.newLine()
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

  def iCalendarFormat(linestoedit: Seq[String]): Map[String, Seq[String]] =
    //randomUUID creates random UUID for the Ics format
    //Date is the key :D
    //Date should be located at index 1
    var empty = Map[String,Seq[String]]()
    // Checks if the linestoEdit is not empty

    if linestoedit.size > 0 then
      var i = 0
      while i < linestoedit.length do
        empty = empty ++ (Map[String,Seq[String]](linestoedit(i+2)-> Seq[String](
        "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//My Calendar Program//Example Corp//EN\nCALSCALE:GREGORIAN\n\nBEGIN:VEVENT\n",
        "UID:" + linestoedit(i)+"\n",
        "DTSTAMP:" + linestoedit(i+1)+"\n",
        "DTSTART:" + linestoedit(i+2) + "z\n",
        "DTEND:" + linestoedit(i+3) + "z\n",
        "SUMMARY:" + linestoedit(i+4) + "\n",
        "END:VEVENT\n\nEND:VCALENDAR\n\n")))
        i += 5
      empty
    else
      empty



  // Reads the whole file and stores the data into a Map.
  // This map can be used to process the data elsewhere.

  def readFile: Map[String,Seq[String]] =
    var storedEvents = Map[String,Seq[String]]()
    var mapKey: String = ""
    val lineReader = try BufferedReader(FileReader("eventInfo.txt"))

      catch
        case e: FileNotFoundException => return storedEvents
        case e: IOException => return storedEvents

    var oneline: String = lineReader.readLine()
    oneline = lineReader.readLine()
    while oneline != null do

      if oneline.contains("UID:") then
        var mapData =  Seq[String]()

        mapData = mapData :+ oneline.drop(4).take(36)
        oneline = lineReader.readLine()
        for i <- 0 until(4) do
          if i == 0 then
            mapData = mapData :+ oneline.drop(8)
          if i == 1 then
            mapData = mapData :+ oneline.drop(8).take(12)
            mapKey = oneline.drop(8).take(12)
          if i == 2 then
            mapData = mapData :+ oneline.drop(6).take(12)
          if i == 3 then
            mapData = mapData :+ oneline.drop(8)


          oneline = lineReader.readLine()
        storedEvents = storedEvents + (mapKey->mapData)
      oneline = lineReader.readLine()
    storedEvents

  //Ongelma : Ei toimi jos yrittää addaa eventin tyhjää txt fileen
  //

  def addEvent(userInput: String) =
    // Generates new UID
    val generateNewUid: String = (UUID.randomUUID().toString + "-1234567890@example.com ,")
    val userInputToSeq: Seq[String] = (generateNewUid ++ userInput).split(",").toSeq
    val formattedUserInput = iCalendarFormat(userInputToSeq)
    if readFile.size > 1 then
      val readFileValues = readFile.map((i)=>i._2).reduce(_++_)
      writetoFile(iCalendarFormat(readFileValues) ++ formattedUserInput)
    else if readFile.size == 1 then
      writetoFile(iCalendarFormat(readFile.values.toSeq(0))++formattedUserInput)
    else
      writetoFile(formattedUserInput)


  def showEventDetails(userInput: String) = readFile(userInput)

// Tarkoitus: Etsi rivi tiedostosta ja muokkaa sitä. Esim käyttäjä syöttää tapahtuman nimen ja mitä hän haluaa muokata. editEvent etsii rivin tiedostosta ja kirjoittaa tämän rivin uudestaan.



  def editEvent(userInput:String ,whatToEdit: (String, String)): Unit =
    var listOfEvents = readFile

    var index = listOfEvents(userInput).indexOf(whatToEdit._1)

    if index == -1 then
      index = listOfEvents(userInput).length-1

    val updatedList = listOfEvents(userInput).updated(index,whatToEdit._2)
    println(userInput)
    println(updatedList)

    listOfEvents = readFile + (userInput -> updatedList)
    println(listOfEvents.values.reduce(_++_))
    writetoFile(iCalendarFormat(listOfEvents.values.reduce(_++_)))



// Tarkoitus : Talenna jokainen rivi muuttujaan, jos rivit eivät sisällä event to deletee, niin kirjoita nämä rivit uudestaan tiedostoon.

// implement method when maptowrite size is
  def deleteEvent(userInput: String) =
    var mapToWrite = Seq[String]()
    if readFile.size > 1 then
      mapToWrite = readFile.-(userInput).values.reduce(_++_)
      writetoFile(iCalendarFormat(mapToWrite))
    else
     writetoFile(Map(""->mapToWrite))

  def getEventName(key: String) =
    readFile(key)(1)
  def getEventEndTime(key: String) =
    readFile(key)(3).dropRight(1)
  def getEventDescription(key: String) =
    readFile(key)(4)


  def showEvents = readFile.keys

  private val dateFormat = DateTimeFormatter.ofPattern("yyyyMMddHHmm")

  def getdayOfWeek(date: String) =
    LocalDateTime.parse(date,dateFormat).getDayOfWeek.getValue

  def getTime(date: String) =
    LocalDateTime.parse(date,dateFormat).getHour
    //LocalDateTime.parse(date,dateFormat).getMinute





}
