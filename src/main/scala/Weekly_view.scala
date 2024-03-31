import Weekly_view.{allEventChildren, stage}
import javafx.collections.FXCollections
import scalafx.stage.*
import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.beans.binding.Bindings
import scalafx.scene.Node
import scalafx.scene.layout.*
import scalafx.scene.control.*
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.event.ActionEvent
import scalafx.scene.input.*
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import sys.process.*
import java.time.{LocalDate, LocalDateTime, LocalTime}
import javafx.util.converter.LocalTimeStringConverter
import scalafx.geometry.Pos

import java.time.format.DateTimeFormatter
import java.util.OptionalInt
import scala.annotation.internal.Child
import akka.actor.*
import scalafx.scene.control.Alert.AlertType

import scala.concurrent.duration.*
import java.time.{Duration, LocalDateTime}

object Weekly_view extends JFXApp3:
  // Define a case class to represent a reminder
 var allEventChildren = List[Node]()
      // Variables for scene size
 var sceneWidth = 800.0
 var sceneHeight = 800.0
     // Define the days of the week
 val days = List[String]("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    // Variables for tracking the current date and events
 var dateTracker = Events.getDateToday
 private val gridpane = new GridPane
        //Helper function for setting things to the grid
 def setEventstoGrid(dateStart: String, dateEnd: String) =
          // Convert date strings to date format
   val convertedDateStart = Events.convertDate(dateStart)
   val convertedDateEnd = Events.convertDate(dateEnd)
          // Get the weekday of the current date
   val weekDay = dateTracker.getDayOfWeek.getValue
          // Get the start date of the current week
   var startOfTheWeekDate = dateTracker.minusDays(weekDay - 1)

          // Initialize the current date
   var currentDate = convertedDateStart
          // Loop through each day of the event
   while currentDate.isBefore(convertedDateEnd) || currentDate.isEqual(convertedDateEnd) do
            // Check if the current date falls within the current week
     if !currentDate.isBefore(startOfTheWeekDate) && currentDate.isBefore(startOfTheWeekDate.plusDays(7)) then
              // Get necessary information such as start time, end time and day of the week
       val x = currentDate.getDayOfWeek.getValue
              // Initialize start and end time variables
       var startTime = LocalTime.MIDNIGHT
       if (currentDate.isEqual(convertedDateStart)) then
         startTime = Events.getTime(dateStart)
       var endTime: LocalTime = LocalTime.MAX
       if currentDate.isEqual(convertedDateEnd) then
         endTime = Events.getTime(dateEnd)
              // Calculate start and end time ratios
       val y = startTime.getHour
       val minStart = startTime.getMinute.toDouble
       val y_2 = endTime.getHour
       val minEnd = endTime.getMinute.toDouble
              // Calculate proportions of total day's height
       val startTimeRatio = (y + minStart / 60) / 25
       val endTimeRatio = (y_2 + minEnd / 60) / 25
              // Calculate total height of the event based on screen height (800)
       val eventHeight = (endTimeRatio - startTimeRatio) * 800
              // Calculate the Y offset to position the event correctly in the grid
       val eventOffset = (minStart / 60) * (sceneHeight / 25)
              // Create a stack pane for the event
       val stack = new StackPane()
       val rectangle = new Rectangle()
       rectangle.width = 800/10
       rectangle.height = eventHeight
       rectangle.fill = Color.Green
              // Align the stack and shift it down
       stack.setAlignment(Pos.TopLeft)
       stack.setTranslateY(eventOffset)
              // Create a tooltip for the event
       val tooltip = new Tooltip()
       tooltip.setText("Event name: " + Events.getEventName(dateStart) + "\n" +
       "Event start time: " + Events.convertDate(dateStart) + "\n" +
       "Event end time: " + Events.convertDate(dateEnd) + "\n" +
       "Event description: " + Events.getEventDescription(dateStart))
              // Add the event name to the stack
       val label = new Label(Events.getEventName(dateStart))
       Tooltip.install(stack,tooltip)
              // Add the rectangle and label to the stack
       stack.getChildren().addAll(rectangle, label)
              // Add the stack to the list of all events
       allEventChildren = allEventChildren.appended(stack)
              // Add the stack to the grid
       gridpane.add(stack, x, y+1)
              // Calculate row span for the event
       var rowSpan = 0
       if minEnd>minStart then
         rowSpan = 1
       val eventRowSpan = y_2 - y + rowSpan
              // Set the row span for the stack in the grid
       GridPane.setRowSpan(stack, eventRowSpan)
            // Move to the next day
     currentDate = currentDate.plusDays(1)

 def deleteEventsFromGrid: Unit =
          // Remove all events from the grid
   allEventChildren.foreach((i)=> gridpane.children -= i)
          // Empty the list of all events
   allEventChildren = allEventChildren.empty

        // Add all events to the grid
   Events.showEvents.foreach((i)=>
   setEventstoGrid(i,Events.getEventEndTime(i)))

 case class Reminder(task: String, when: Duration)

  // Define an Actor for handling reminders

  class ReminderActor extends Actor {
    import context.dispatcher

    // The receive method handles incoming messages to this actor
    def receive = {
      // When receiving a Reminder, schedule the task to be done after a specified duration
      case Reminder(task, when) =>
        val duration = scala.concurrent.duration.Duration.fromNanos(when.toNanos).asInstanceOf[FiniteDuration]
        context.system.scheduler.scheduleOnce(duration, self, task)
      // When receiving a task, print a reminder
      case (task) =>
        println("Remember your" + task)
    }
  }
  // Create an ActorSystem and an actor of type ReminderActor
  val system = ActorSystem("ReminderSystem")
  val reminderActor = system.actorOf(Props(new ReminderActor), "reminderActor")
  private val timeFormat = DateTimeFormatter.ofPattern("HH:mm:ss")


  // Define a start function to set up a weekly calendar

  def start(): Unit =

    // Define the stage
    stage = new JFXApp3.PrimaryStage{
      title = "Calendar"
      scene = new Scene(sceneWidth,sceneHeight){


        //Creates a gridpane layout.
        // Enable grid lines for debugging purposes
        gridpane.gridLinesVisible = true

        //Column width persentage set to 100/8 (7 days + time column + buttons)
        val columnWidthPercentage = 100 / 10
        for i <- 0 until(10) do
          val column = new ColumnConstraints()
          column.percentWidth = columnWidthPercentage
          gridpane.getColumnConstraints().add(column)
        //add date label
        var date = new Label(dateTracker.toString)
        gridpane.add(date,0,0)


        val weekDay = dateTracker.getDayOfWeek.getValue
        val firstDayOfTheWeek = dateTracker.minusDays(weekDay)
        //add labels for each day of the week
        for i <- days.indices do
          val check = publIcHolidays.findHoliday(Events.getDateOnly(firstDayOfTheWeek.plusDays(i+1)))
          val vbox = new VBox()
          val label = new Label(days(i))
          vbox.getChildren.add(label)

          if check.nonEmpty then
            val label2 = new Label(check)
            vbox.getChildren.add(label2)
          gridpane.add(vbox,i+1,0)


        //add buttons to move between weeks
        val button1 = new Button("<--")
        val button2 = new Button("-->")
        gridpane.add(button1,8,0)
        gridpane.add(button2,9,0)

        //define actions for the buttons to navigate between weeks
        button1.onAction = (e: ActionEvent) => {
          dateTracker = dateTracker.minusWeeks(1)
          date.text = dateTracker.toString
          deleteEventsFromGrid
          Events.showEvents.foreach((i)=>setEventstoGrid(i,Events.getEventEndTime(i)))

        }
        button2.onAction = (e: ActionEvent) => {
          dateTracker = dateTracker.plusWeeks(1)
          date.text = dateTracker.toString
          deleteEventsFromGrid
          Events.showEvents.foreach((i)=>setEventstoGrid(i,Events.getEventEndTime(i)))


        }
        // set up time slots
        var clock = 0.00
        //Time slots will be set to 60 mins. So rowsHeightPercetage will be 100 / (24) on default
        val rowsHeightPercentage = 100 / 24
        //HEIGHT
        for i <- 0 until(24) do
            val row = new RowConstraints()
            row.percentHeight = rowsHeightPercentage
            gridpane.getRowConstraints.add(row)
            val label = new Label(clock.toString)

            gridpane.add(label,0,i+1)
            clock += 1

        root = gridpane

        val contextmenu = new ContextMenu()
        contextmenu.items.add((new MenuItem("Add events"){onAction = () => Dialogs.EventInputDialog()
        deleteEventsFromGrid}))
        contextmenu.items.add((new MenuItem("Edit events"){onAction = () => Dialogs.editEventDialog}))
        contextmenu.items.add((new MenuItem("Delete events"){onAction = () => Dialogs.deleteDialog}))
        contextmenu.items.add(new MenuItem("Filter events"){onAction = () => Dialogs.categoriesDialog})
        //contextmenu.items.add(new MenuItem("add Categirues"){onAction = () => addCategoryDialog})

        onMouseClicked = (me: MouseEvent) =>
          if me.button == MouseButton.Secondary then
            contextmenu.show(this.window(),me.screenX,me.screenY)


      }
    }
