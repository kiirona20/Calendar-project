import scalafx.scene.paint.Color

import java.time.Duration.between
import java.time.LocalDateTime
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
//Sets up alarm for posibility to change it
private var alarm: Alarm = null

class Event(var uid: String = "", var startTime: String = "",
            var endTime: String = "", var summary: Option[String] = None,
            var description: Option[String] = None, var categories: Option[String] = None,
            var color: Option[Color] = None,
            var trigger: Option[String] = None) {
  defineAlarm
  //Finds the value what to edit
  def findAndEdit(element: String, change: Option[String]): Unit =
    val check = change.isDefined
    element match
      case "startTime" => if check then startTime = change.get
      case "endTime" => if check then endTime = change.get
      case "summary" => summary = change
      case "description" => description = change
      case "categories" => categories = change
      case "trigger" =>
        trigger = change
        //cancels the scheduler if alarm is defined
        if alarm != null then
          alarm.alarmEvent.cancel()
          defineAlarm
        else
          defineAlarm
      case _ => println("Element not found or cannot be changed")
  def changeColor(color1: Option[Color]) =
    color = color1
    println(color)
  
  
  //Checks if the alarm is defined
  def defineAlarm: Unit =
    if trigger.isDefined then
      val triggerTime = Events.convertStringToDateTIme(trigger.get)
      //Calculates the between time Using java.Time library
      val javaDuration = between(LocalDateTime.now(), triggerTime)
      //Converts it to scala finiteDuration type
      val scalaDuration = FiniteDuration.apply(javaDuration.toSeconds, TimeUnit.SECONDS)
      //Check that there are no alarms that has passed already and if the alarm hasn't been defined yet
      if scalaDuration.>=(FiniteDuration.apply(1, TimeUnit.SECONDS)) then
        alarm = Alarm(scalaDuration, summary.getOrElse("No name for the task"))
      //If alarm has been defined cancel the scheduler and make a new alarm
      //else if scalaDuration.>=(FiniteDuration.apply(1, TimeUnit.SECONDS)) then
        //alarm.alarmEvent.cancel()
        //((alarm = Alarm(scalaDuration, summary.getOrElse("No name for the task"))
      else
        println("Some error in defining the alarm")
}

