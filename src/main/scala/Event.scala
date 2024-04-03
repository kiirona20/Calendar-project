import java.time.Duration.between
import java.time.LocalDateTime
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration

class Event(var uid:String = "", var startTime: String = "",
            var endTime: String = "", var summary: Option[String] = None,
            var description: Option[String] = None, var categories: Option[String] = None,
            var trigger: Option[String] = None){

  def findAndEdit(element: String, change: Option[String]): Unit =
    val check = change.isDefined
    element match
      case "startTime" => if check then startTime = change.get
      case "endTime" => if check then endTime = change.get
      case "summary" => summary = change
      case "description" => description = change
      case "categories" => categories = change
      case "trigger" => trigger = change
      case _ => println("Element not found or cannot be changed")
      
  //Checks if the alarm is defined    
  if trigger.isDefined then
    val triggerTime = Events.convertDateTime(trigger.get)
    //Calculates the between time Using java.Time library
    val javaDuration = between(LocalDateTime.now(),triggerTime)
    //Converts it to scala finiteDuration type
    val scalaDuration = FiniteDuration.apply(javaDuration.toSeconds, TimeUnit.SECONDS)
    //Check that there are no alarms that has passed already
    if scalaDuration.>=(FiniteDuration.apply(1,TimeUnit.SECONDS)) then
      Alarm(scalaDuration, summary.getOrElse("No name for the task"))
  }

