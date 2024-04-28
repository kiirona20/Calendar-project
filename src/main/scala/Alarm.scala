// Import necessary libraries and packages
import akka.actor.{Actor, ActorSystem, Props}
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.application.Platform
import java.time.Duration.between
import java.time.temporal.TemporalAmount
import java.time.LocalDateTime
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

// Define a class Alarm that takes trigger time and task name as parameters
class Alarm(trigger: FiniteDuration, taskName: String) {

  // Define an internal Actor to handle the alarm task
  object someActor extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      // When receiving a FiniteDuration message
      case s: FiniteDuration =>
        // Show an Alert dialog with taskName as a reminder
        Platform.runLater(new Alert(AlertType.Information, "Reminder of task: " + taskName).showAndWait())
        // Terminate the actor system
        context.system.terminate()
    }
  }

  // Create an ActorSystem with the name "Alarm"
  val system = ActorSystem("Alarm")

  // Create an actor instance based on the defined Actor
  val actor = system.actorOf(Props(someActor), "Alarm")

  // Implicit ExecutionContext needed for scheduling
  implicit val ec: ExecutionContext = system.dispatcher

  // Schedule the alarm event to be triggered after the specified trigger time
  val alarmEvent = system.scheduler.scheduleOnce(trigger)(actor ! trigger)
}