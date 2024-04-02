
import Events.{getDateOnly, getDateToday, getdayOfWeek, groupedByCategories}
import Weekly_view.dateTracker

import java.time.LocalDateTime
import scala.io.StdIn.*

object Main extends App {
    //Should work with this input = name,202003031700,202003031800, hopefully this works :D

 /** println("Choose to add an event, edit event or delete an event")
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


*/


  val testEvent = "20230512160000,20230513190000, hopefully this works :(, kiva, CONFERENCE,20230512110000"
  val testEvent2 = "20230513180000,20230513190000, hopefully this works :(, mörkö, CONFERENCE,20230512120000"
  val testEvent3 = "20230512140000,20230512160000, hopefully this works :(, kokki, CONFERENCE,20230512130000"
  val testEvent4 = "20230512190000,20230303200000, hopefully this works :(, emt, CONFERENCE,  "
  val event12 = Event( "jotain1", "20240322160000","20240322190000", Some("joe"), Some("joe"), Some("tapaaminen"), Some(Events.convertLocalDateTimeToFormat(LocalDateTime.now.plusSeconds(7))))
  val event13 = Event( "jotain2", "20240323160000","20240323190000", None, None, None, None)
  val event14 = Event( "jotain1", "20240324160000","20240322190000", Some("lol"), Some("mörkö"), Some("tapaaminen"), Some(Events.convertLocalDateTimeToFormat(LocalDateTime.now.plusSeconds(10))))
  val testFilter = Seq("tapaaminen")
  val event15 = Event( "mopo", "20240424160000","20240422190000", Some("mopo"), Some("mopo"), Some("mopo"), None)
  val eventAlarm = Event("mopo", "20240424162000","20240422190000", Some("mopo"), Some("mopo"), Some("mopo"), Some(Events.convertLocalDateTimeToFormat(LocalDateTime.now.plusSeconds(5))))


@main
  def alarm =
    eventAlarm

//@main
  //def testWriting =
    //Events.writetoFile(Events.iCalendarFormat(List("4d5ca2fe-5b81-45a5-afb0-66cb28f39373", "name", "20230512T150000z", "20230512T200000z",  "hopefully this works :D")))
@main
  def testReadingBetter = println(Events.readFileBetter)


@main
  def testEditBetter =
    Events.editEventBetter("20240323160000","description",Some("lol"))

@main
  def showEvents =
    println(Events.showEvents)
    calendarState.appliedFilters(testFilter)
    println(calendarState.appliedFilter)
    println(Events.groupedByCategories.filter((i) => calendarState.appliedFilter.contains(i._1.get)).values.flatMap((i) => i.keys))
    println(Events.showEvents)



@main
  def testAdding = Events.addEventBetter(event15)// start time should be unique

@main
  def testIcalendar =
    //println(Events.iCalendarFormat(Events.readFile.map((i)=>i._2).reduce((a,b)=>a++b)))
    println(Events.iCalderFormatBetter(Events.readFileBetter.values.toSeq))

@main
  def testDelete =
    Events.deleteEvent("20240323160000")
@main
  def converDAte =
    println(Events.convertDate("20240324190000"))

@main
  def testdateformat =
    println(Events.getdayOfWeek("20230303170000")) // Friday

@main
  def testtimeformat =
    println(Events.getHour("20230303170000"))
@main
  def testCategories =
    println(Events.allCategories)
    println(Events.groupedByCategories)

@main
  def testGetTime =
    println(Events.getTime("20240324190000"))
@main
  def testgetInfo =
    println(Events.getEventName("20240322160000"))
    println(Events.getEventDescription("20240322160000"))
    println(Events.getEventEndTime("20240322160000"))

    println(Events.getMin("20240322160000"))
    println(Events.getTime("20240322160000").toString)
    println(Events.groupedByCategories)
    println(Events.allCategories)}

@main
  def testGettingDayRight =
    println(getdayOfWeek("20240101160000"))
    println(getDateToday.getDayOfWeek)
@main
  def checkHolidays =
    val today = dateTracker
    val weekDay = today.getDayOfWeek.getValue
    val firstDayOfTheWeek = today.minusDays(weekDay)
    for i <- 1to(7) do
      println(publIcHolidays.findHoliday(getDateOnly(firstDayOfTheWeek.plusDays(i))))







