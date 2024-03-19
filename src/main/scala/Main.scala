import Main.userInput

import scala.io.StdIn.*

object Main extends App {
    //Should work with this input = name,202003031700,202003031800, hopefully this works :D

  println("Choose to add an event, edit event or delete an event")
  var userInput = readLine()

  if userInput.toLowerCase == "add" then
    println("Write the event name, date, starTime, endTime and description. Seperate these by comma ','")
    userInput = readLine()
    Events.addEvent(userInput)


  if userInput.toLowerCase == "edit" then
    println("Which event do you wish to edit\nList of recorded events: " + Events.readFile.keys)
    userInput = readLine()
    println("What do you wish to edit?" + Events.showEventDetails(userInput))
    var userInput2 = readLine()
    println("To what do you wish it to change?")
    var userInput3 = readLine()
    val stuffChanged = Events.editEvent(userInput,(userInput2,userInput3))



  if userInput.toLowerCase == "delete" then
    println("Which event do you wish to delete\nList of recorded events: " + Events.readFile.keys)
    userInput = readLine()
    Events.deleteEvent(userInput)
    println(Events.readFile)}





  val testEvent = "20230512160000,20230513190000, hopefully this works :(, kiva, CONFERENCE,20230512110000"
  val testEvent2 = "20230513180000,20230513190000, hopefully this works :(, mörkö, CONFERENCE,20230512120000"
  val testEvent3 = "20230512140000,20230512160000, hopefully this works :(, kokki, CONFERENCE,20230512130000"
  val testEvent4 = "20230512190000,20230303200000, hopefully this works :(, emt, CONFERENCE,  "




//@main
  //def testWriting =
    //Events.writetoFile(Events.iCalendarFormat(List("4d5ca2fe-5b81-45a5-afb0-66cb28f39373", "name", "20230512T150000z", "20230512T200000z",  "hopefully this works :D")))
@main
  def testReadingBetter = println(Events.readFileBetter)


@main
  def testReading = println(Events.readFile)

@main
  def testAdding = Events.addEvent(testEvent4) // start time should be unique

@main
  def testIcalendar =
    println(Events.iCalendarFormat(Events.readFile.map((i)=>i._2).reduce((a,b)=>a++b)))
    Events.iCalderFormatBetter(Events.readFile.map((i)=>i._2).reduce((a,b)=>a++b))

@main
  def testDelete =
    Events.deleteEvent("20230512150000")
@main
  def testEdit =
    Events.editEvent("20230515101010", ("None", "jotain1"))
@main
  def testdateformat =
    println(Events.getdayOfWeek("202303031700")) // Friday

@main
  def testtimeformat =
    println(Events.getHour("202303031700"))
@main
  def testgetInfo =
    //println(Events.getEventName("20230513110000"))
    //println(Events.getEventDescription("20230513110000"))
    //println(Events.getEventEndTime("20230513110000"))

    println(Events.getMin("20230513110000"))
    println(Events.getTime("20230513110000").toString)
    println(Events.groupedByCategories)
    println(Events.allCategories)









