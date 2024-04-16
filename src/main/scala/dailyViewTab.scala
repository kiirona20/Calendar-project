import Weekly_view.{dateTracker, sceneHeight, sceneWidth, setEventToGridWeekly, start}
import scalafx.event.ActionEvent
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.{Button, Label, Tab, TabPane, Tooltip}
import scalafx.scene.layout.GridPane.{getColumnSpan, setColumnSpan, setConstraints}
import scalafx.scene.layout.{ColumnConstraints, GridPane, RowConstraints, StackPane}
import scalafx.Includes.*
import scalafx.geometry.Pos
import scalafx.scene.input.MouseDragEvent
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import java.time.LocalTime

object dailyViewTab{

  var currentDay = Events.getDateToday
  val rowsHeightPercentage = 100 / 25
  //width set 1 day + 2 buttons
  //day takes 80% of the screen
  //each button takes 10%
  val columnWidthPercentage = 80
  val columnWidthforButtons = 10
  var clock = 0.00

  val gridPane = new GridPane
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
  column1.setPercentWidth(columnWidthforButtons)
  column1.setMaxWidth(sceneWidth/10)
  column1.setMinWidth(sceneWidth/10)
  column1.setPrefWidth(sceneWidth/10)
  gridPane.getColumnConstraints.add(column1)
  val button1 = new Button("<--")
  val column2 = new ColumnConstraints()
  column2.setPercentWidth(columnWidthPercentage)
  column2.setMaxWidth(sceneWidth/1.25)
  column2.setMinWidth(sceneWidth/1.25)
  column2.setPrefWidth(sceneWidth/1.25)
  gridPane.getColumnConstraints.add(column2)
  val column3 = new ColumnConstraints()
  column3.setPercentWidth(columnWidthforButtons)
  column3.setMaxWidth(sceneWidth/10)
  column3.setMinWidth(sceneWidth/10)
  column3.setPrefWidth(sceneWidth/10)
  gridPane.getColumnConstraints.add(column3)


  val button2 = new Button("-->")
  gridPane.add(button1, 0, 0)


  //FindHoliday in case the publicholiday is on the same day you open the calendar
  val day = new Label(currentDay.getDayOfWeek.toString + "  " + publIcHolidays.findHoliday(Events.getDateOnly(currentDay)))
  gridPane.add(day,1,0)
  gridPane.add(button2,2,0)


  button1.onAction = (e: ActionEvent) =>
    currentDay = currentDay.minusDays(1)
    val holidays = publIcHolidays.findHoliday(Events.getDateOnly(currentDay))
    day.text = currentDay.getDayOfWeek.toString + "  " + holidays
    View.deleteEventsFromGrid

  button2.onAction = (e: ActionEvent) =>
    currentDay = currentDay.plusDays(1)
    val holidays = publIcHolidays.findHoliday(Events.getDateOnly(currentDay))
    day.text = currentDay.getDayOfWeek.toString + "  " + holidays
    View.deleteEventsFromGrid

  println(gridPane.children)

  Events.showEvents.foreach((i)=>SetEventToGridDaily(i,Events.getEventEndTime(i)))


//Same as weeklySetEventsTogrid but considers only 1 day 
  def SetEventToGridDaily(dateStart: String, dateEnd: String) =
    //Converts dateStart and dateEnd to localDateTime for making it easier to compare these dates
    var convertedDateStart = Events.convertStringToDate(dateStart)
    val convertedDateEnd = Events.convertStringToDate(dateEnd)
    //Loops through the days
    var currentDayLoop = convertedDateStart

    while currentDayLoop.isBefore(convertedDateEnd) || currentDayLoop.isEqual(convertedDateEnd) do
      if currentDayLoop.isEqual(currentDay) then


        var startTime = LocalTime.MIDNIGHT
        if currentDay.isEqual(convertedDateStart) then
          startTime = Events.convertStringToTime(dateStart)
        var endtime = LocalTime.MAX
        if currentDay.isEqual(convertedDateEnd) then
          endtime = Events.convertStringToTime(dateEnd)

        val hourStart = startTime.getHour
        val minuteStart = startTime.getMinute.toDouble
        val hourEnd = endtime.getHour
        val minuteENd = endtime.getMinute.toDouble

        val startTimeRatio = (hourStart+minuteStart/60)/24
        val endTimeRatio = (hourEnd+minuteENd/60)/24
        val eventHeight = (endTimeRatio-startTimeRatio)*sceneHeight
        val eventOffset = (minuteStart / 60) * (sceneHeight / 3)


        var rowSpanMinute = 0
        if minuteENd>minuteStart then
           rowSpanMinute = 1

        val eventRowSpan = hourEnd - hourStart + rowSpanMinute

        val stack = new StackPane()
        val rectangle = new Rectangle()

        rectangle.width = sceneWidth/1.25-10
        rectangle.height = eventHeight
        rectangle.fill = Events.getEventColor(dateStart)
        stack.setAlignment(Pos.TopLeft)
        stack.setTranslateY(eventOffset)
        val label = new Label(Events.getEventName(dateStart))
        stack.getChildren.addAll(rectangle, label)

        View.allEventChildren = View.allEventChildren.appended(stack)

        gridPane.add(stack,1,hourStart+1)
        GridPane.setRowSpan(stack,eventRowSpan)

      currentDayLoop = currentDayLoop.plusDays(1)







}












