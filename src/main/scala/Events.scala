

import java.io.{BufferedReader, BufferedWriter, FileNotFoundException, FileReader, FileWriter, IOException, PrintWriter}
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


  def writetoFile(linesToWrite: Map[String, Seq[String]]):  Unit =
    val fileIn = FileWriter("eventInfo.ics") // set to true so the writing will append to a file
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

  def checkAlarm(alarm: String) =
    try
      LocalDateTime.parse(alarm,dateFormat)
      true
    catch
      case ex: Exception => false

  def iCalendarFormat(linestoedit: Seq[String]): Map[String, Seq[String]] =
    //randomUUID creates random UUID for the Ics format
    //Date is the key :D
    //Date should be located at index 1
    var empty = Map[String,Seq[String]]()

    // Checks if the linestoEdit is not empty
    var i = 0
    println(linestoedit(i+6))
    if linestoedit.size > 0 then
      while i < linestoedit.length do
        val alarm = if linestoedit.length >= i+6 && checkAlarm(linestoedit(i+6)) then "BEGIN:VALARM\nACTION:AUDIO\n" +
          "TRIGGER:" + linestoedit(i+6) + "z\n"+ "END:VALARM\n" else ""
        println(alarm)
        empty = empty ++ (Map[String,Seq[String]](linestoedit(i+1)-> Seq[String](
        "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//My Calendar Program//Example Corp//EN\nCALSCALE:GREGORIAN\n\nBEGIN:VEVENT\n",
        "UID:" + linestoedit(i)+"\n",
        "DTSTAMP:" + LocalDateTime.now().toString.replace("-","").replace(":","").takeWhile((i)=>i != '.') + "z\n",
        "DTSTART:" + linestoedit(i+1) + "z\n",
        "DTEND:" + linestoedit(i+2) + "z\n",
        "SUMMARY:" + linestoedit(i+3) + "\n",
        "CATEGORIES:" + linestoedit(i+4) + "\n",
        "DESCRIPTION:" + linestoedit(i+5) + "\n",
         alarm,
        "END:VEVENT\n\nEND:VCALENDAR\n\n")))
        i += 7
      empty
    else
      empty



  // Reads the whole file and stores the data into a Map.
  // This map can be used to process the data elsewhere.

  def readFile: Map[String,Seq[String]] =
    var storedEvents = Map[String,Seq[String]]()
    var mapKey: String = ""
    val lineReader = try BufferedReader(FileReader("eventInfo.ics"))

      catch
        case e: FileNotFoundException => return storedEvents
        case e: IOException => return storedEvents

    var oneline: String = lineReader.readLine()
    var mapData =  (Seq[String](),Seq[Int]())
    oneline = lineReader.readLine()
    while oneline != null do


      if oneline.contains("UID:") then
        //adds UID
        val data = (oneline.slice(4, 40),1)
        mapData = (mapData._1 :+ data._1, mapData._2 :+ data._2)
     // if oneline.contains("DTSTAMP:") then
          // adds DTSTAMP
       // val data = (oneline.slice(8,16) + oneline.slice(17,23),2)
       // mapData = (mapData._1 :+ data._1, mapData._2 :+ data._2)
      if oneline.contains("DTSTART:") then
        // adds DTStart format: Dropping( DASTART:) taking (8 char Year/month/day) dropping 1 char and taking (6 char, Hour/min/seconds)
        val data = (oneline.slice(8,16) + oneline.slice(17,23),2)
        mapData = (mapData._1 :+ data._1, mapData._2 :+ data._2)
        mapKey = oneline.slice(8,16) + oneline.slice(17,23)
      if oneline.contains("DTEND:") then
        // adds DTSEND Format
        val data = (oneline.slice(6,14) + oneline.slice(15,21),3)
        mapData = (mapData._1 :+ data._1, mapData._2 :+ data._2)
      if oneline.contains("SUMMARY:") then
        //adds Summary
        val data = (oneline.drop(8),4)
        mapData = (mapData._1 :+ data._1, mapData._2 :+ data._2)
      if oneline.contains("CATEGORIES:") then
        val data = (oneline.drop(11),5)
        mapData = (mapData._1 :+ data._1, mapData._2 :+ data._2)
      if oneline.contains("DESCRIPTION:") then
        val data = (oneline.drop(12),6)
        mapData = (mapData._1 :+ data._1, mapData._2 :+ data._2)
      if oneline.contains("TRIGGER:") then
        val data = (oneline.slice(8,16) + oneline.slice(17,23),7)
        mapData = (mapData._1 :+ data._1, mapData._2 :+ data._2)



      oneline = lineReader.readLine()
    //zipping seq[String] and Seq[Int] and the sorting them by Int
    //storedEvents will always have the same order, so it will be easier to manipulate data afterwards.
    storedEvents = storedEvents + (mapKey->mapData._1.zip(mapData._2).sortBy(_._2).map(_._1))
    storedEvents




  def addEvent(userInput: String) =
    // Generates new UID
    val generateNewUid: String = (UUID.randomUUID().toString + "-1234567890@example.com ,")
    val userInputToSeq: Seq[String] = (generateNewUid ++ userInput).split(",").toSeq
    val formattedUserInput = iCalendarFormat(userInputToSeq)
    println(formattedUserInput)
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
    if !listOfEvents.contains(userInput) then
      println("Event not found!")
      return

    var index = listOfEvents(userInput).indexWhere(_.contains(whatToEdit._1))
    if (index == -1) then
      println(s"Attribute '${whatToEdit._1}' not found in the event.")
      return

    val updatedList = listOfEvents(userInput).updated(index, whatToEdit._2)

    listOfEvents = listOfEvents.updated(userInput, updatedList)
    println(listOfEvents)
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
    readFile(key)(3)

  def getEventEndTime(key: String) =
    readFile(key)(2)
  def getEventDescription(key: String) =
    readFile(key)(5)
  // 0 = UID 1 = start time 2 = end time 3 = summary  4 = categories 5 = description 6 = alarm

  def showEvents = readFile.keys

  private val dateFormat = DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
  private val dateFormat2 = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")

  def getdayOfWeek(date: String) =
    LocalDateTime.parse(date,dateFormat).getDayOfWeek.getValue

  def getHour(date: String) =
    println(LocalDateTime.parse(date,dateFormat))
    LocalDateTime.parse(date,dateFormat).getHour
  def getTime(date: String) =
    LocalTime.parse(date,dateFormat)

  def getMin(date: String) = 
    LocalDateTime.parse(date,dateFormat).getMinute

  def convertDate(date: String) =
    LocalDateTime.parse(date,dateFormat).toLocalDate

  def getDateToday = LocalDate.now()








}
