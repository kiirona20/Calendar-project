import Weekly_view.{dateTracker, gridpane, sceneHeight, sceneWidth, setEventToGridWeekly, start}
import javafx.geometry.HPos
import scalafx.scene.input.MouseButton
import scalafx.event.ActionEvent
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.{Button, Label, Tab, TabPane, Tooltip}
import scalafx.scene.layout.GridPane.{getColumnSpan, setColumnSpan, setConstraints}
import scalafx.scene.layout.{ColumnConstraints, GridPane, RowConstraints, StackPane}
import scalafx.Includes.*
import scalafx.geometry.Pos
import scalafx.scene.input.{MouseDragEvent, MouseEvent}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import scala.math.{max, min}
import java.time.LocalTime
import java.time.format.DateTimeFormatter

object dailyViewTab{
  var dragInputCheck = false
  var currentDay = dateTimeHandler.getDateToday
  val rowsHeightPercentage = 100 / 25
  //width set 1 day + 2 buttons
  //day takes 80% of the screen
  //each button takes 10%
  val columnWidthPercentage = 80
  val columnWidthforButtons = 10
  var clock = 0.00
  private val timeFormat = DateTimeFormatter.ofPattern("HH:mm:ss")


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
  val day = new Label(currentDay.getDayOfWeek.toString + "  " + publIcHolidays.findHoliday(dateTimeHandler.getDateOnly(currentDay)) + "  " + currentDay)
  gridPane.add(day,1,0)
  gridPane.add(button2,2,0)


  button1.onAction = (e: ActionEvent) =>
    currentDay = currentDay.minusDays(1)
    val holidays = publIcHolidays.findHoliday(dateTimeHandler.getDateOnly(currentDay))
    day.text = currentDay.getDayOfWeek.toString + "  " + holidays + "  " + currentDay
    View.deleteEventsFromGrid

  button2.onAction = (e: ActionEvent) =>
    currentDay = currentDay.plusDays(1)
    val holidays = publIcHolidays.findHoliday(dateTimeHandler.getDateOnly(currentDay))
    day.text = currentDay.getDayOfWeek.toString + "  " + holidays+ "  " + currentDay
    View.deleteEventsFromGrid

  var startMousePressed = 0.0
  var endMouseReleased = 0.0
  gridPane.onMousePressed = (e:MouseEvent) =>
    if e.button == MouseButton.Primary then
      startMousePressed = e.getY

  gridPane.onMouseReleased = (e: MouseEvent) =>
    if e.button == MouseButton.Primary then
      endMouseReleased = e.getY
      dragInputCheck = true

      dragCalculateTime
      dragInputCheck = false

  var startTime:String = ""
  var endTime:String = ""
  def dragCalculateTime =
    val oneRowHeight = sceneHeight/24
    //-1 beacause there are 25 collumns
    val startTimeHour = ((min(startMousePressed,endMouseReleased)/oneRowHeight).floor-1).toInt
    val endTimeHour = ((max(startMousePressed,endMouseReleased)/oneRowHeight).floor-1).toInt

    val startTimeMinute = (min(startMousePressed,endMouseReleased)%oneRowHeight)/oneRowHeight*60
    val endTimeMinute = (max(endMouseReleased,startMousePressed)%oneRowHeight)/oneRowHeight*60
    println("Event StartTime: Hour: " + startTimeHour + "Mins:" + startTimeMinute + "Event EndTime: hour: " + endTimeHour + "mins" + endTimeMinute)
    if startTimeHour < 0 then println("event starting time has to start form 0.0")
    else
      startTime = LocalTime.of(startTimeHour.toInt, startTimeMinute.toInt, 0).format(timeFormat)
      endTime = LocalTime.of(endTimeHour, endTimeMinute.toInt, 0).format(timeFormat)
      Dialogs.EventInputDialog
  EventHandler.showEvents.foreach((i)=>SetEventToGridDaily(i,EventHandler.getEventEndTime(i)))



//Same as weeklySetEventsTogrid but considers only 1 day 
  def SetEventToGridDaily(dateStart: String, dateEnd: String) =
    //println("Grid daily dateStart:" + Events.convertStringToDateTIme(dateStart))
    //println("Grid daily dateEnd:" + Events.convertStringToDateTIme(dateEnd))
    //Converts dateStart and dateEnd to localDateTime for making it easier to compare these dates
    var convertedDateStart = dateTimeHandler.convertStringToDate(dateStart)
    val convertedDateEnd = dateTimeHandler.convertStringToDate(dateEnd)
    //Loops through the days
    var currentDayLoop = convertedDateStart

    while currentDayLoop.isBefore(convertedDateEnd) || currentDayLoop.isEqual(convertedDateEnd) do
      if currentDayLoop.isEqual(currentDay) then


        var startTime = LocalTime.MIDNIGHT
        if currentDay.isEqual(convertedDateStart) then
          startTime = dateTimeHandler.convertStringToTime(dateStart)
        var endtime = LocalTime.MAX
        if currentDay.isEqual(convertedDateEnd) then
          endtime = dateTimeHandler.convertStringToTime(dateEnd)

        val hourStart = startTime.getHour
        val minuteStart = startTime.getMinute.toDouble
        val hourEnd = endtime.getHour
        val minuteENd = endtime.getMinute.toDouble

        val startTimeRatio = (hourStart+minuteStart/60)/24
        val endTimeRatio = (hourEnd+minuteENd/60)/24
        val eventHeight = (endTimeRatio-startTimeRatio)*sceneHeight
        val eventOffset = (minuteStart / 60) * (sceneHeight / Weekly_view.amountOfColumnds)


        var rowSpanMinute = 0
        if minuteENd>minuteStart then
           rowSpanMinute = 1

        val eventRowSpan = hourEnd - hourStart + rowSpanMinute

        val stack = new StackPane()
        val rectangle = new Rectangle()

        rectangle.width = sceneWidth/1.25-10
        rectangle.height = eventHeight
        rectangle.fill = EventHandler.getEventColor(dateStart)
        stack.setAlignment(Pos.TopLeft)
        stack.setTranslateY(eventOffset)
        val label = new Label(EventHandler.getEventName(dateStart))
        stack.getChildren.addAll(rectangle, label)

        View.allEventChildren = View.allEventChildren.appended(stack)

        gridPane.add(stack,1,hourStart+1)
        GridPane.setRowSpan(stack,eventRowSpan)

      currentDayLoop = currentDayLoop.plusDays(1)







}












