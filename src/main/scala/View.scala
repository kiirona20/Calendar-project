import Weekly_view.{dateTracker, sceneHeight, sceneWidth}
import scalafx.geometry.Pos
import scalafx.scene.Node
import scalafx.scene.control.Label
import scalafx.scene.layout.{GridPane, StackPane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import java.time.LocalTime
import java.time.format.DateTimeFormatter

object View {
  var allEventChildren = List[Node]()
  // Variables for scene size
  var sceneWidth = Weekly_view.sceneWidth
  var sceneHeight = Weekly_view.sceneHeight
  // Define the days of the week
  val days = List[String]("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  // Variables for tracking the current date and events


  def putWeekdaysAndHolidays: Unit =
    val weekDay = Weekly_view.dateTracker.getDayOfWeek.getValue
    val firstDayOfTheWeek = Weekly_view.dateTracker.minusDays(weekDay)
    //add labels for each day of the week
    for i <- days.indices do
      val check = publIcHolidays.findHoliday(dateTimeHandler.getDateOnly(firstDayOfTheWeek.plusDays(i + 1)))
      val label = new Label()
      if check.nonEmpty then
        label.setText(days(i) + "   " + check)
        println("This is holiday:")
      else
        label.setText(days(i))
      allEventChildren = allEventChildren.appended(label)

      Weekly_view.gridpane.add(label, i + 1, 0)

 def deleteEventsFromGrid: Unit =

   allEventChildren.foreach((i)=> Weekly_view.gridpane.children -= i)
   allEventChildren.foreach((i)=> dailyViewTab.gridPane.children -= i)
   allEventChildren = allEventChildren.empty
   EventHandler.showEvents.foreach((i)=>Weekly_view.setEventToGridWeekly(i,EventHandler.getEventEndTime(i)))
   EventHandler.showEvents.foreach((i)=>dailyViewTab.SetEventToGridDaily(i,EventHandler.getEventEndTime(i)))
   putWeekdaysAndHolidays


}
