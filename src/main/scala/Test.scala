
import dateTimeHandler.{getDateOnly, getdayOfWeek}
import Weekly_view.dateTracker
import scalafx.scene.paint.Color

import java.time.{LocalDate, LocalDateTime}
import scala.io.StdIn.*

object Test extends App {
  //BackendTesting

  val testEvent = "20230512160000,20230513190000, hopefully this works :(, kiva, CONFERENCE,20230512110000"
  val testEvent2 = "20230513180000,20230513190000, hopefully this works :(, mörkö, CONFERENCE,20230512120000"
  val testEvent3 = "20230512140000,20230512160000, hopefully this works :(, kokki, CONFERENCE,20230512130000"
  val testEvent4 = "20230512190000,20230303200000, hopefully this works :(, emt, CONFERENCE,  "
  val event12 = Event( "jotain1", "20240322160000","20240322190000", Some("joe"), Some("joe"), Some("tapaaminen"),None, Some(dateTimeHandler.convertLocalDateTimeToFormat(LocalDateTime.now.plusSeconds(7))))
  val event13 = Event( "jotain2", "20240323160000","20240323190000", None, None, None, None)
  val event14 = Event( "jotain1", "20240324160000","20240322190000", Some("lol"), Some("mörkö"), Some("tapaaminen"),None, Some(dateTimeHandler.convertLocalDateTimeToFormat(LocalDateTime.now.minusSeconds(10))))
  val testFilter = Seq("tapaaminen")
  val event15 = Event( "mopo", "20240424160000","20240422190000", Some("mopo"), Some("mopo"), Some("mopo"), Some(Color.Green))
  val eventAlarm = Event("mopo", "20240424162000","20240422190000", Some("mopo"), Some("mopo"), Some("mopo"), Some(Color.Green),Some(dateTimeHandler.convertLocalDateTimeToFormat(LocalDateTime.now.plusSeconds(15))))


//@main
  //def alarm =
    //eventAlarm

//@main
  //def testWriting =
    //Events.writetoFile(Events.iCalendarFormat(List("4d5ca2fe-5b81-45a5-afb0-66cb28f39373", "name", "20230512T150000z", "20230512T200000z",  "hopefully this works :D")))
@main
  def testReadingBetter = println(EventHandler.readFile)


@main
  def testEditBetter =
    EventHandler.editEvent("20240323160000","description",Some("lol"))

@main
  def showEvents =
    println(EventHandler.showEvents)
    calendarState.appliedFilters(testFilter)
    println(calendarState.appliedFilter)
    println(EventHandler.groupedByCategories.filter((i) => calendarState.appliedFilter.contains(i._1.get)).values.flatMap((i) => i.keys))
    println(EventHandler.showEvents)

@main
  def AlarmTest = 
    eventAlarm
    

@main
  def testAdding = EventHandler.addEvent(eventAlarm)
  println(eventAlarm.color)// start time should be unique

@main
  def testIcalendar =
    //println(Events.iCalendarFormat(Events.readFile.map((i)=>i._2).reduce((a,b)=>a++b)))
    println(EventHandler.iCalendarformat(EventHandler.readFile.values.toSeq))

@main
  def testDelete =
    EventHandler.deleteEvent("20240323160000")
@main
  def converDAte =
    println(dateTimeHandler.convertStringToDate("20240324190000"))

@main
  def testdateformat =
    println(dateTimeHandler.getdayOfWeek("20230303170000")) // Friday

@main
  def testtimeformat =
    println(dateTimeHandler.getHour("20230303170000"))
@main
  def testCategories =
    println(EventHandler.allCategories)
    println(EventHandler.groupedByCategories)

@main
  def testGetTime =
    println(dateTimeHandler.convertStringToTime("20240324190000"))
@main
  def testgetInfo =
    println(EventHandler.getEventName("20240322160000"))
    println(EventHandler.getEventDescription("20240322160000"))
    println(EventHandler.getEventEndTime("20240322160000"))

    println(dateTimeHandler.getMin("20240322160000"))
    println(dateTimeHandler.convertStringToTime("20240322160000").toString)
    println(EventHandler.groupedByCategories)
    println(EventHandler.allCategories)}

@main
  def testGettingDayRight =
    println(getdayOfWeek("20240101160000"))
    println(LocalDate.now())
@main
  def checkHolidays =
    val today = dateTracker
    val weekDay = today.getDayOfWeek.getValue
    val firstDayOfTheWeek = today.minusDays(weekDay)
    for i <- 1to(7) do
      println(publIcHolidays.findHoliday(getDateOnly(firstDayOfTheWeek.plusDays(i))))







