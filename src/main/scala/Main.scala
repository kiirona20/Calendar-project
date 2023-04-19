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





  val testEvent = "name,202305121500,202003032000, hopefully this works :D"



@main
  def testWriting =
    Events.writetoFile(Events.iCalendarFormat(List("4d5ca2fe-5b81-45a5-afb0-66cb28f39373", "name", "202003031700z", "202003031800z",  "hopefully this works :D")))
@main
  def testReading = println(Events.readFile)

@main
  def testAdding = Events.addEvent(testEvent) // start time should be unique

@main
  def testIcalendar =
    println(Events.iCalendarFormat(Events.readFile.map((i)=>i._2).reduce((a,b)=>a++b)))

@main
  def testDelete =
    Events.deleteEvent("202003031900")
@main
  def testEdit =
    Events.editEvent("202003031700",("hopefully this works :D","jotain"))
@main
  def testdateformat =
    println(Events.getdayOfWeek("202303031700")) // Friday

@main
  def testtimeformat =
    println(Events.getTime("202303031700"))
@main
  def testgetInfo =
    println(Events.getEventName("202001021700"))
    println(Events.getEventDescription("202001021700"))
    println(Events.getEventEndTime("202001021700"))










