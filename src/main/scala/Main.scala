import Main.userInput

import scala.io.StdIn.*

object Main extends App {
    //Should work with this input = name,202003031700,202003031800, hopefully this works :D

  println("Choose to add an event, edit event or delete an event")
  var userInput = readLine()
  if userInput.toLowerCase == "add" then
    println("Write the event name, date, starTime, endTime and description. Seperate these by comma ','")
    userInput = readLine()
    Events(userInput).addEvent


  if userInput.toLowerCase == "edit" then
    println("Which event do you wish to edit\nList of recorded events: " + Events(userInput).readFile.keys)
    userInput = readLine()
    println("What do you wish to edit?" + Events(userInput).showEventDetails)
    var userInput2 = readLine()
    println("To what do you wish it to change?")
    var userInput3 = readLine()
    val stuffChanged = Events(userInput).editEvent(userInput2,userInput3)



  if userInput.toLowerCase == "delete" then
    println("Which event do you wish to delete\nList of recorded events: " + Events(userInput).readFile.keys)
    userInput = readLine()
    Events(userInput).deleteEvent
    println(Events(userInput).readFile)}





  val testEvent = Events("name,202003031700,202003032000, hopefully this works :D")



@main
  def testWriting =
    testEvent.writetoFile(testEvent.iCalendarFormat(List("4d5ca2fe-5b81-45a5-afb0-66cb28f39373", "name", "202003031700z", "202003031800z",  "hopefully this works :D")))
@main
  def testReading = println(testEvent.readFile)

@main
  def testAdding = testEvent.addEvent // start time should be unique

@main
  def testIcalendar =
    println(testEvent.iCalendarFormat(testEvent.readFile.map((i)=>i._2).reduce((a,b)=>a++b)))

@main
  def testDelete =
    Events("202003031800").deleteEvent
@main
  def testEdit =
    Events("202003031700").editEvent("202003031700", "222222222222")








