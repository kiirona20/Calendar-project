
import akka.actor.{Actor, ActorSystem, Props}

import java.time.Duration.between
import java.time.temporal.TemporalAmount
import java.time.LocalDateTime
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*



class Alarm(trigger: FiniteDuration, taskName: String) {  
  case class alarmDifference(alarmSetTrigger: LocalDateTime)
  object someActor extends Actor {
    def receive = {
      case s:FiniteDuration =>
        println("Time: " + s)
        println("Name of the task: " + taskName + "\n")
        context.system.terminate()
    }
  }
    val system = ActorSystem("Alarm")
    val actor = system.actorOf(Props(someActor),"Alarm")
    implicit val ec: ExecutionContext = system.dispatcher
    system.scheduler.scheduleOnce(trigger)(actor ! trigger)

  
}

