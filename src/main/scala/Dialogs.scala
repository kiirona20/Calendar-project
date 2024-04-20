import Events.allCategories
import Weekly_view.{gridpane, setEventToGridWeekly}
import javafx.scene.layout.VBox
import scalafx.Includes.{jfxBooleanBinding2sfx, jfxBooleanProperty2sfx}
import scalafx.beans.property.BooleanProperty
import scalafx.Includes.*

import scala.compiletime.ops.boolean.||
import javafx.util.converter.LocalTimeStringConverter
import scalafx.event.ActionEvent
import scalafx.scene.control.{Alert, Button, ButtonType, CheckBox, ColorPicker, ComboBox, DatePicker, Dialog, Label, ListView, TextField, TextFormatter}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.input.KeyCode.N
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color
import scalafx.stage.Stage.sfxStage2jfx

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDate, LocalDateTime}
import java.util.UUID

object Dialogs {
  //Result class to store are dialog values
  case class Result(name: String, startDate: String, startTime: String, endDate: String, endTime: String, description: String, category: String, color: Color, alarmDate: String, alarmTime: String)
  private val generateNewUid: String = (UUID.randomUUID().toString + "-1234567890@example.com ,")
  // Function to display a dialog for adding a new even
  def EventInputDialog: Unit =
    // Create a new dialog for adding events
    val dialog = new Dialog[Result]()
    dialog.setTitle("Add Event")
    dialog.setHeaderText("Enter event details")
    val startDateText = if dailyViewTab.dragInputCheck then dailyViewTab.currentDay else LocalDate.now
    val startTimeText = if dailyViewTab.dragInputCheck then dailyViewTab.startTime else null
    val endDateText = if dailyViewTab.dragInputCheck then dailyViewTab.currentDay else LocalDate.now
    val endTimeText = if dailyViewTab.dragInputCheck then dailyViewTab.endTime else null


    // Define the format for the time fields
    val timePattern = "HH:mm:ss"

    // Create input fields for event details
    val nameLabel = new Label("Event Name:")
    val nameInput = new TextField()
    val startDateLabel = new Label("Start Date:")
    val startDateInput = new DatePicker(startDateText)
    val startTimeLable = new Label("Start Time:")
    val startTimeInput = new TextField()
    startTimeInput.text = startTimeText
    // Set the format for the start time field
    startTimeInput.promptText = "Put time in format HH:mm:ss"

    // Disable editing of the date fields
    startDateInput.getEditor.setDisable(true)
    val endDateLabel = new Label("End Date:")
    val endDateInput = new DatePicker(endDateText)
    endDateInput.getEditor.setDisable(true)

    // Create end time field
    val endTimeLable = new Label("End Time:")
    val endTimeInput = new TextField()
    endTimeInput.text = endTimeText
    // Set the format for the end time field
    endTimeInput.promptText = "Put time in format HH:mm:ss"

    // Create input fields for event description and category
    val descriptionLabel = new Label("Description:")
    val descriptionInput = new TextField()

    val categoryLabel = new Label("Category:")
    val categoryInput = new TextField()

    val colorLabel = new Label("Color of the event:")
    val colorpicker = new ColorPicker()

    // Create input fields for alarm date and time
    val alarmdateLabel = new Label("Alarm date")
    val alarmdateInput = new DatePicker()

    val alarmtimeLabel = new Label("Alarm time")
    val alarmtimeInput = new TextField()


    //add stuff to grid
    val grid = new GridPane()
    grid.add(nameLabel, 1, 1)
    grid.add(nameInput, 2, 1)
    grid.add(startDateLabel, 1, 2)
    grid.add(startDateInput, 2, 2)
    grid.add(startTimeLable, 1, 3)
    grid.add(startTimeInput, 2, 3)


    grid.add(endDateLabel, 1, 4)
    grid.add(endDateInput, 2, 4)
    grid.add(endTimeLable, 1, 5)
    grid.add(endTimeInput, 2, 5)

    grid.add(descriptionLabel, 1, 6)
    grid.add(descriptionInput, 2, 6)

    grid.add(categoryLabel, 1, 7)
    grid.add(categoryInput, 2, 7)

    grid.add(colorLabel,1,8)
    grid.add(colorpicker,2,8)


    grid.add(alarmdateLabel, 1, 9)
    grid.add(alarmdateInput, 2, 9)

    grid.add(alarmtimeLabel, 1, 10)
    grid.add(alarmtimeInput, 2, 10)



    dialog.getDialogPane.setContent(grid)

    //Setting up buttons
    val submitButtonType = new ButtonType("Submit", ButtonData.OKDone)
    dialog.getDialogPane.getButtonTypes.addAll(submitButtonType, ButtonType.Cancel)
    val submitButton = dialog.dialogPane().lookupButton(submitButtonType)
    // Disables the submit button if any of the necessary fields are empty
    // '<==' Binds the right side to the left side so that whenever the value is true from right it disbles the button
    submitButton.disableProperty() <== startDateInput.getEditor.textProperty.isEmpty || endDateInput.getEditor.textProperty.isEmpty
      || startTimeInput.text.isEmpty || endTimeInput.text.isEmpty


    dialog.resultConverter = button =>
      if button == submitButtonType then
        Result(nameInput.getText,
          startDateInput.getValue.toString, startTimeInput.getText,
          endDateInput.getValue.toString, endTimeInput.getText,
          descriptionInput.getText, categoryInput.getText,
          colorpicker.getValue,
          if alarmdateInput.getValue != null then alarmdateInput.getValue.toString else "", alarmtimeInput.getText)
      else
        null

    val result = dialog.showAndWait()

    result match {
      case Some(Result(n, s1, s2, e1, e2, d, c,c2, a1, a2)) => println("name=" + n + "\nstartDate="
        + s1 + ", startTime=" + s2 + "\nendDate=" + e1 + ", endTime=" + e2 +
        "\ndescription=" + d + "Color" + c2 +
        "\nalarmDate=" + a1 + "  alarmTime=" + a2)
        userInput(n, s1, s2, e1, e2, d, c,c2, a1, a2)
      case Some(_) => println("ErROR :DD")
      case None => println("Dialog returned: None")
    }

  def userInput(summary: String, dateStart: String, timeStart: String, dateEnd: String, timeEnd: String, description: String, category: String, color: Color, alarmDate: String, alarmTime: String) =
    //val category: Option[String] = if description.nonEmpty then Some(category) else None
    //val categories = None
    val stDate: String = dateStart.replace("-", "")
    val stTime: String = timeStart.replace(":", "")
    val stDateTime = stDate + stTime
    val endDate = dateEnd.replace("-", "")
    val endTime = timeEnd.replace(":", "")
    val endDateTime = endDate + endTime
    val alarDate = if alarmDate.nonEmpty then alarmDate.replace("-", "") else ""
    val alarTime = if alarmTime.nonEmpty then alarmTime.replace(":", "") else ""
    val category1 = if category.nonEmpty then Some(category) else None

    val summary1 = if summary.isEmpty then None else Some(summary)
    val description1 = if description.isEmpty then None else Some(description)
    val alarmDateTIme: Option[String] = if alarDate.nonEmpty && alarTime.nonEmpty then Some(alarDate + alarTime) else None
    val event = Event(generateNewUid, stDateTime, endDateTime, summary1, description1, category1, Some(color), alarmDateTIme)
    Events.addEventBetter(Event(generateNewUid, stDateTime, endDateTime, summary1, description1, category1, Some(color), alarmDateTIme))
    View.deleteEventsFromGrid
    println(event.color)
    println(event.description)

  def editEventDialog: Unit =
    val dialog = new Dialog[Unit]()
    dialog.setTitle("Edit event")
    dialog.setHeaderText("Click on the event you wish to edit and press edit Button")

    var listView = new ListView(Events.showEventsEditDialog.toList.map((i)=>Events.readFileBetter(i).startTime +"   " + Events.getEventName(i)))

    listView.prefHeight = 200
    listView.prefWidth = 200
    val grid = new GridPane()
    val checkboxes = allCategories.map((i) => new CheckBox(i.get)).toSeq
    val vBox = VBox()
    val filterLabel = new Label("Filter list by categories")
    val filterButton = new Button("Filter")
    vBox.getChildren.add(filterLabel)
    checkboxes.foreach((i)=>vBox.getChildren.add(i))


    grid.add(listView, 1, 1)
    grid.add(vBox,2,1)
    dialog.getDialogPane.setContent(grid)
    filterButton.onAction = (e:ActionEvent) =>
      handleCheckBox(checkboxes, true)
      //listView.items = (Events.showEventsEditDialog.map((i)=>Events.readFileBetter(i).startTime +"   " + Events.getEventName(i)))
    val editButton = new Button("Edit")
    grid.add(editButton, 1, 2)
    grid.add(filterButton,2,2)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.Cancel, ButtonType.Finish)
    editButton.onAction = (e: ActionEvent) =>
      val selected = listView.selectionModel.apply().getSelectedItem.slice(0,14)
      if selected != null then


        processEditDialog(selected.toString)
    dialog.showAndWait()

  def processEditDialog(key: String) =

    val dialog = new Dialog[Unit]()
    val grid = new GridPane()
    dialog.setTitle("Editor")
    dialog.setHeaderText("Choose what you want to edit")


    val timePattern = "HH:mm:ss"
    val format = DateTimeFormatter.ofPattern(timePattern)
    //val timeConverter = new LocalTimeStringConverter(format, format)


    val nameLabel = new Label("Event Name:")
    val nameInput = new TextField()
    nameInput.setText(Events.getEventName(key))
    val startDateLabel = new Label("Start Date:")
    val startDateInput = new DatePicker(Events.convertStringToDate(key))
    val startTimeLable = new Label("Start Time:")
    val startTimeInput = new TextField()
    //startTimeInput.textFormatter = new TextFormatter(timeConverter)
    startTimeInput.setText(Events.convertStringToTime(key).format(format))
    println("Event start time" + Events.convertStringToTime(key).format(format))

    //forces user to use specific format :D
    //startTimeInput.textFormatter = new TextFormatter(timeConverter)
    startDateInput.getEditor.setDisable(true)
    val endDateLabel = new Label("End Time:")
    val endDateInput = new DatePicker(Events.convertStringToDate(Events.getEventEndTime(key)))
    println("Event end Date:" + Events.getEventEndTime(key))
    endDateInput.getEditor.setDisable(true)

    val endTimeLable = new Label("End Time:")
    val endTimeInput = new TextField()
    //endTimeInput.textFormatter = new TextFormatter(timeConverter)
    println("Event end time:" + Events.convertStringToTime(Events.getEventEndTime(key)).format(format))

    endTimeInput.setText(Events.convertStringToTime(Events.getEventEndTime(key)).format(format))


    val descriptionLabel = new Label("Description:")
    val descriptionInput = new TextField()
    descriptionInput.setText(Events.getEventDescription(key))


    val categoryLabel = new Label("Category: COMING SOONG :D")
    val categoryInput = new TextField()
    categoryInput.setText(Events.getCategorie(key))


    // Create input fields for alarm date and time
    val alarmdateLabel = new Label("Alarm date")
    val alarmdateInput = new DatePicker()
    //Events.convertStringToDate(Events.getAlarm(key))

    val alarmtimeLabel = new Label("Alarm time")
    val alarmtimeInput = new TextField()
    //alarmtimeInput.setText(Events.convertStringToTime(Events.getAlarm(key)).toString)

    val colorpicker = new ColorPicker()

    println("Event description:" + Events.getEventDescription(key))

    grid.add(nameLabel, 1, 1)
    grid.add(nameInput, 2, 1)
    grid.add(startDateLabel, 1, 2)
    grid.add(startDateInput, 2, 2)
    grid.add(startTimeLable, 1, 3)
    grid.add(startTimeInput, 2, 3)


    grid.add(endDateLabel, 1, 4)
    grid.add(endDateInput, 2, 4)
    grid.add(endTimeLable, 1, 5)
    grid.add(endTimeInput, 2, 5)

    grid.add(descriptionLabel, 1, 6)
    grid.add(descriptionInput, 2, 6)

    grid.add(categoryLabel, 1, 7)
    grid.add(categoryInput, 2, 7)



    grid.add(alarmdateLabel, 1, 8)
    grid.add(alarmdateInput, 2, 8)

    grid.add(alarmtimeLabel, 1, 9)
    grid.add(alarmtimeInput, 2, 9)

    dialog.getDialogPane.setContent(grid)

    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.Cancel)

    val updateButton = new Button("Update")
    grid.add(updateButton, 2, 10)


    updateButton.onAction = (e: ActionEvent) =>
      val newName: Option[String] = if nameInput.text != null then Some(nameInput.getText) else None
      val newStartDate: Option[String] = Some(startDateInput.getValue.toString.replace("-", "") + startTimeInput.getText.replace(":", ""))
      val newEndDate: Option[String] = Some(endDateInput.getValue.toString.replace("-", "") + endTimeInput.getText.replace(":", ""))
      val newDescription: Option[String] = if descriptionInput.text != null then Some(descriptionInput.getText) else None
      val newCategory: Option[String] = if categoryInput.text != null then Some(categoryInput.getText) else None
      val newAlarm: Option[String] = if alarmdateInput.getEditor.getText.nonEmpty && alarmtimeInput.getText.nonEmpty then
        Some(alarmdateInput.getValue.toString.replace("-", "") + alarmtimeInput.getText.replace(":", "")) else None

      Events.editEventBetter(key, "startTime", newStartDate)
      Events.editEventBetter(key, "endTime", newEndDate)
      Events.editEventBetter(key, "description", newDescription)
      Events.editEventBetter(key, "summary", newName)
      Events.editEventBetter(key, "categories", newCategory)
      Events.editEventBetter(key, "trigger", newAlarm)
      dialog.close()


    dialog.showAndWait()
    View.deleteEventsFromGrid

  def deleteDialog: Unit =
    val dialog = new Dialog[Unit]()
    dialog.setTitle("Delete event")
    dialog.setHeaderText("Click on Events you wish to delete and then press Delete Button")

    val listView = new ListView(Events.showAllEvents.toList.map((i)=>Events.readFileBetter(i).startTime +"   " + Events.getEventName(i)))
    listView.prefHeight = 200
    listView.prefWidth = 200


    val grid = new GridPane()
    grid.add(listView, 1, 1)
    dialog.getDialogPane.setContent(grid)
    val deleteButton = new Button("Delete")
    grid.add(deleteButton, 1, 2)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.Cancel, ButtonType.Finish)
    deleteButton.onAction = (e: ActionEvent) =>
      val selected = listView.selectionModel.apply().getSelectedItem
      if selected != null then
        Events.deleteEvent(selected.slice(0,14))
        View.deleteEventsFromGrid
      //Events.showEvents.foreach((i)=>Weekly_view.setEventtoGrid(i,Events.getEventEndTime(i), Weekly_view.gridpane, true))
      listView.items = listView.items.apply().diff(Seq(selected))


    dialog.showAndWait()


  def categoriesDialog: Unit =
    val dialog = new Dialog[Unit]()
    dialog.setTitle("Categories")
    dialog.setHeaderText("Here you can filter events by categories.")
    val button = new Button("filter")
    //
    // List of all categories
    val allCategories = Events.allCategories
    val eventsGroupedByCategories = Events.groupedByCategories

    // Create checkboxes for all categories
    val checkboxes = allCategories.map((i) =>
      new CheckBox(i.get)
    ).toSeq

    button.onAction = (e: ActionEvent) =>
      handleCheckBox(checkboxes,false)
    val grid = new GridPane()
    for i <- checkboxes.indices do
      grid.add(checkboxes(i), i, 0)
    grid.add(button, 0, 1)
    dialog.getDialogPane.setContent(grid)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.Cancel, ButtonType.Finish)
    dialog.showAndWait()
    View.deleteEventsFromGrid

  def handleCheckBox(boxes: Seq[CheckBox], filterList: Boolean) =
    var selectedList: Seq[String] = Seq[String]()
    for i <- boxes.indices do
      if boxes(i).isSelected then
        selectedList = selectedList :+ boxes(i).getText
    if filterList then
      calendarState.appliedFiltersDialog(selectedList)
    else
      calendarState.appliedFilters(selectedList)



}