
import java.time.temporal.TemporalAmount
import java.time.{Duration, LocalDateTime}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


class Alarm(alarm: LocalDateTime) {
  val timeNow: LocalDateTime = LocalDateTime.now
  val d = Duration.between(LocalDateTime.now(),alarm)
  Thread.sleep(d)
  println("Something Happens :D")
  
}

