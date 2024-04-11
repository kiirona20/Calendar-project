import Weekly_view.gridpane
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, Tab, TabPane}
import scalafx.scene.layout.GridPane.{getColumnSpan, setColumnSpan, setConstraints}
import scalafx.scene.layout.{ColumnConstraints, GridPane, RowConstraints}
import scalafx.Includes.*

object dailyViewTab{
  private var currentDay = Events.getDateToday
  val rowsHeightPercentage = 100 / 25
  //width set 1 day + 2 buttons
  //day takes 80% of the screen
  //each button takes 10%
  val columnWidthPercentage = 100 / 1.25
  val columnWidthforButtons = 100 / 10
  var clock = 0.00

  val gridPane = new GridPane()
  for i <- 0 until (24) do
    val row = new RowConstraints()
    row.percentHeight = rowsHeightPercentage
    gridPane.getRowConstraints.add(row)
    val label = new Label(clock.toString)
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



  def setEventsToGridDaily(dateStart: String, dateEnd: String) =
    val convertedDateStart = Events.convertStringToDateTIme(dateStart)
    val convertedDateEnd = Events.convertStringToDateTIme(dateStart)


    // Loop through each day of the event
    while convertedDateStart.isBefore(convertedDateEnd) || convertedDateStart.isEqual(convertedDateEnd) do ???










}












