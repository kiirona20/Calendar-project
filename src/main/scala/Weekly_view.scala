
import javafx.util.converter.LocalTimeStringConverter
import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.beans.binding.Bindings
import scalafx.event.ActionEvent
import scalafx.geometry.Pos
import scalafx.scene.control.*
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.input.*
import scalafx.scene.layout.*
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.{Node, Scene}
import scalafx.stage.*

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDate, LocalDateTime, LocalTime}
import java.util.OptionalInt
import scala.annotation.internal.Child
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.sys.process.*

object Weekly_view extends JFXApp3:

 // Variables for scene size
 var sceneWidth = 0.0
 var sceneHeight = 0.0
 val amountOfRows = 10
 val amountOfColumnds = 25

 // Variable for tracking the current date
 var dateTracker = LocalDate.now()


 val gridpane = new GridPane


  //Helper function for setting things to the grid
  def setEventToGridWeekly(dateStart: String, dateEnd: String) =
    // Convert date strings to date format
   val convertedDateStart = dateTimeHandler.convertStringToDate(dateStart)
   val convertedDateEnd = dateTimeHandler.convertStringToDate(dateEnd)
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
       // Variable to place the event in the correct row
       val x = currentDate.getDayOfWeek.getValue
       // Initialize start and end time variables
       var startTime = LocalTime.MIDNIGHT
       var endTime: LocalTime = LocalTime.MAX
       // Checks if the currentDate is same as starting date
       // If not then startTime starts at 00:00. This is in case of events that lasts longer than 1 day
       if currentDate.isEqual(convertedDateStart) then
         startTime = dateTimeHandler.convertStringToTime(dateStart)
       // Chekcs if the currentDate is same as end Date
       // If not then endTime is 23:59. This is in case of events that lasts longer than 1 day
       if currentDate.isEqual(convertedDateEnd) then
         endTime = dateTimeHandler.convertStringToTime(dateEnd)
       // Initialize time variables
       val hourStart = startTime.getHour
       val minStart = startTime.getMinute.toDouble
       val hourEnd = endTime.getHour
       val minuteEnd = endTime.getMinute.toDouble
        // Calculate start and end time ratios
       val startTimeRatio = (hourStart + minStart / 60) / 24
       val endTimeRatio = (hourEnd + minuteEnd / 60) / 24
       // Calculate total height of the event based on screen height
       val eventHeight = (endTimeRatio - startTimeRatio) * sceneHeight
       // Calculate the Y offset to position the event correctly in the grid
       val eventOffset = (minStart / 60) * (sceneHeight / 24)
       // Calculate row span for the event

       var rowSpanMinute = 0
       if minuteEnd>minStart then
         rowSpanMinute = 1
       //how many cells does the event occupy
       val eventRowSpan = hourEnd - hourStart + rowSpanMinute
       // Create a stack pane for the event
       val stack = new StackPane()
       val rectangle = new Rectangle()
       //Sets width depending on is it daily- or weeklyview
       rectangle.width = sceneWidth/amountOfRows-10
       rectangle.height = eventHeight
       rectangle.fill = EventHandler.getEventColor(dateStart)
       // Align the stack and shift it down
       stack.setAlignment(Pos.TopLeft)
       stack.setTranslateY(eventOffset)
       // Create a tooltip for the event
       val tooltip = new Tooltip()
       tooltip.setText("Event name: " + EventHandler.getEventName(dateStart) + "\n" +
       "Event start time: " + dateTimeHandler.convertStringToDateTIme(dateStart) + "\n" +
       "Event end time: " + dateTimeHandler.convertStringToDateTIme(dateEnd) + "\n" +
       "Event description: " + EventHandler.getEventDescription(dateStart))
       // Add the event name to the stack
       val label = new Label(EventHandler.getEventName(dateStart))
       Tooltip.install(stack,tooltip)
       // Add the rectangle and label to the stack
       stack.getChildren.addAll(rectangle, label)
       // Add the stack to the list of all events
       View.allEventChildren = View.allEventChildren.appended(stack)
       // Add the stack to the grid
       gridpane.add(stack, x, hourStart+1)
       // Set the row span for the stack in the grid
       GridPane.setRowSpan(stack, eventRowSpan)
     // Move to the next day
     currentDate = currentDate.plusDays(1)
  // Define a start function
  def start(): Unit =
    // Define the stage
    stage = new JFXApp3.PrimaryStage{
      title = "Calendar"
      val bounds = Screen.primary.getVisualBounds
      sceneWidth = bounds.getWidth - 10
      sceneHeight = bounds.getHeight - 80

      scene = new Scene(sceneWidth,sceneHeight){
        sceneHeight = sceneHeight-10
        private val tabPane = new TabPane
        private val tab1 = new Tab
        private val tab2 = new Tab
        tab1.setText("WeeklyView")
        tab2.setText("DailyView")

        //Makes gridLinesVisible
        gridpane.gridLinesVisible = true

        //Column width persentage set to 100/10 (7 days + time column + buttons)
        val columnWidthPercentage = 100 / 10
        for i <- 0 until(10) do
          val column = new ColumnConstraints()
          column.percentWidth = columnWidthPercentage
          gridpane.getColumnConstraints.add(column)
        //add date label

        val rowsHeightPercentage = 100 / 25
        val constraint = new RowConstraints
        constraint.setPercentHeight(rowsHeightPercentage)
        constraint.setPrefHeight(sceneHeight/25)
        constraint.setMaxHeight(sceneHeight/25)
        constraint.setMinHeight(sceneHeight/25)
        gridpane.getRowConstraints.add(constraint)

        var date = new Label(dateTracker.toString)
        gridpane.add(date,0,0)



        View.putWeekdaysAndHolidays


        //add buttons to move between weeks
        val button1 = new Button("<--")
        val button2 = new Button("-->")
        gridpane.add(button1,8,0)
        gridpane.add(button2,9,0)

        //define actions for the buttons to navigate between weeks
        button1.onAction = (e: ActionEvent) =>
          dateTracker = dateTracker.minusWeeks(1)
          date.text = dateTracker.toString
          View.deleteEventsFromGrid
        button2.onAction = (e: ActionEvent) =>
          dateTracker = dateTracker.plusWeeks(1)
          date.text = dateTracker.toString
          View.deleteEventsFromGrid

        // set up time slots
        var clock = 0.00
        //Time slots will be set to 60 mins. So rowsHeightPercetage will be 100 / (24) on default
        //HEIGHT
        for i <- 0 until(24) do
            val row = new RowConstraints()
            row.percentHeight = rowsHeightPercentage
            row.setMaxHeight(sceneHeight/25)
            row.setMinHeight(sceneHeight/25)
            row.setPrefHeight(sceneHeight/25)

            gridpane.getRowConstraints.add(row)
            val label = new Label(clock.toString)

            gridpane.add(label,0,i+1)
            clock += 1


        val contextmenu = new ContextMenu()
        contextmenu.items.add((new MenuItem("Add events"){onAction = () => Dialogs.EventInputDialog}))
        contextmenu.items.add((new MenuItem("Edit events"){onAction = () => Dialogs.editEventDialog}))
        contextmenu.items.add((new MenuItem("Delete events"){onAction = () => Dialogs.deleteDialog}))
        contextmenu.items.add(new MenuItem("Filter events"){onAction = () => Dialogs.categoriesDialog})

        tab1.content = gridpane
        tab2.content = dailyViewTab.gridPane
        tabPane.onMouseClicked = (me: MouseEvent) =>
          if me.button == MouseButton.Secondary then
            contextmenu.show(this.window(),me.screenX,me.screenY)
        //set Events to grid
        EventHandler.showEvents.foreach((i)=>setEventToGridWeekly(i,EventHandler.getEventEndTime(i)))
        tabPane.tabs = List(tab1,tab2)

        root = tabPane
      }
      maximized = true

    }

