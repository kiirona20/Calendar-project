import Weekly_view.{ dateTracker, sceneHeight, sceneWidth}
import scalafx.event.ActionEvent
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.{Button, Label, Tab, TabPane, Tooltip}
import scalafx.scene.layout.GridPane.{getColumnSpan, setColumnSpan, setConstraints}
import scalafx.scene.layout.{ColumnConstraints, GridPane, RowConstraints, StackPane}
import scalafx.Includes.*
import scalafx.geometry.Pos
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import java.time.LocalTime

object dailyViewTab{
  var allEventChildren = List[Node]()

  private var currentDay = Events.getDateToday
  val rowsHeightPercentage = 100 / 25
  //width set 1 day + 2 buttons
  //day takes 80% of the screen
  //each button takes 10%
  val columnWidthPercentage = 100 / 1.25
  val columnWidthforButtons = 100 / 10
  var clock = 0.00

  val gridPane = new GridPane()
  for i <- 0 until (25) do
    val row = new RowConstraints()
    row.percentHeight = rowsHeightPercentage
    gridPane.getRowConstraints.add(row)
    val label = new Label(clock.toString)
    row.setMaxHeight(sceneHeight/25)
    row.setMinHeight(sceneHeight/25)
    row.setPrefHeight(sceneHeight/25)
    if i < 24 then
      gridPane.add(label, 0, i + 1)
    gridPane.gridLinesVisible = true
    clock += 1

  val column1 = new ColumnConstraints()
  column1.percentWidth = columnWidthforButtons

  gridPane.getColumnConstraints.add(column1)
  val button1 = new Button("<--")
  val column2 = new ColumnConstraints()
  column2.percentWidth = columnWidthPercentage
  gridPane.getColumnConstraints.add(column2)
  val column3 = new ColumnConstraints()
  column3.percentWidth = columnWidthforButtons
  gridPane.getColumnConstraints.add(column3)


  val button2 = new Button("-->")
  gridPane.add(button1, 0, 0)



  val day = new Label(currentDay.getDayOfWeek.toString)
  gridPane.add(day,1,0)
  gridPane.add(button2,2,0)

  button1.onAction = (e: ActionEvent) =>
    currentDay = currentDay.minusDays(1)
    day.text = currentDay.getDayOfWeek.toString

  button2.onAction = (e: ActionEvent) =>
    currentDay = currentDay.plusDays(1)
    day.text = currentDay.getDayOfWeek.toString


  Events.showEvents.foreach((i)=>setEventtoGrid(i,Events.getEventEndTime(i)))


 def setEventtoGrid(dateStart: String, dateEnd: String) =
          // Convert date strings to date format
   val convertedDateStart = Events.convertStringToDate(dateStart)
   val convertedDateEnd = Events.convertStringToDate(dateEnd)
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
       println("loL")
              // Initialize start and end time variables
       var startTime = LocalTime.MIDNIGHT
       // Checks if the currentDate is same as starting date
       // If not then startTime starts at 00:00. This is in case of events that lasts longer than 1 day
       if currentDate.isEqual(convertedDateStart) then
         startTime = Events.convertStringToTime(dateStart)

       var endTime: LocalTime = LocalTime.MAX

       // Chekcs if the currentDate is same as end Date
       // If not then endTime is 23:59. This is in case of events that lasts longer than 1 day
       if currentDate.isEqual(convertedDateEnd) then
         endTime = Events.convertStringToTime(dateEnd)
       // Calculate start and end time ratios
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
       val eventOffset = (minStart / 60) * (sceneHeight / Weekly_view.amountOfColumnds)


       // Calculate row span for the event
       var rowSpanMinute = 0
       if minuteEnd>minStart then
         rowSpanMinute = 1
       //how many cells does the event occupy
       val eventRowSpan = hourEnd - hourStart + rowSpanMinute

       // Create a stack pane for the event

       val stack = new StackPane()
       val rectangle = new Rectangle()
       rectangle.width = sceneWidth/3-10

       rectangle.height = eventHeight
       rectangle.fill = Color.Green
              // Align the stack and shift it down
       stack.setAlignment(Pos.TopLeft)
       stack.setTranslateY(eventOffset) //Ehkä ongelma
              // Create a tooltip for the event
       val tooltip = new Tooltip()
       tooltip.setText("Event name: " + Events.getEventName(dateStart) + "\n" +
       "Event start time: " + Events.convertStringToDateTIme(dateStart) + "\n" +
       "Event end time: " + Events.convertStringToDateTIme(dateEnd) + "\n" +
       "Event description: " + Events.getEventDescription(dateStart))
              // Add the event name to the stack
       val label = new Label(Events.getEventName(dateStart))
       Tooltip.install(stack,tooltip)
              // Add the rectangle and label to the stack
       stack.getChildren.addAll(rectangle, label)
              // Add the stack to the list of all events
       allEventChildren = allEventChildren.appended(stack)
              // Add the stack to the grid
       gridPane.add(stack, 1, hourStart+1)



       //
              // Set the row span for the stack in the grid
       GridPane.setRowSpan(stack, eventRowSpan)

            // Move to the next day
     currentDate = currentDate.plusDays(1)









}












