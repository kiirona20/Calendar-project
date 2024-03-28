import Weekly_view.{gridpane, setEventstoGrid}
import scalafx.Includes.{jfxBooleanBinding2sfx, jfxBooleanProperty2sfx}
import scalafx.beans.property.BooleanProperty
import scalafx.Includes.*

import scala.compiletime.ops.boolean.||
import javafx.util.converter.LocalTimeStringConverter
import scalafx.event.ActionEvent
import scalafx.scene.control.{Alert, Button, ButtonType, CheckBox, ComboBox, DatePicker, Dialog, Label, ListView, TextField, TextFormatter}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.input.KeyCode.N
import scalafx.scene.layout.GridPane
import scalafx.stage.Stage.sfxStage2jfx

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDate, LocalDateTime}
import java.util.UUID

object Dialogs {
  //Result class to store are dialog values
  case class Result(name: String, startDate: String, startTime: String, endDate: String, endTime: String, description: String, alarmDate: String, alarmTime: String)
  private val generateNewUid: String = (UUID.randomUUID().toString + "-1234567890@example.com ,")
  // Function to display a dialog for adding a new event

  def EventInputDialog(): Unit =
          // Create a new dialog for adding events
          val dialog = new Dialog[Result]()
          dialog.setTitle("Add Event")
          dialog.setHeaderText("Enter event details")

          // Define the format for the time fields
          val timePattern = "HH:mm:ss"

          // Create input fields for event details
          val nameLabel = new Label("Event Name:")
          val nameInput = new TextField()
          val startDateLabel = new Label("Start Date:")
          val startDateInput = new DatePicker(LocalDate.now)
          val startTimeLable = new Label("Start Time:")
          val startTimeInput = new TextField()
          // Set the format for the start time field
          startTimeInput.promptText = "Put time in format HH:mm:ss"

          // Disable editing of the date fields
          startDateInput.getEditor.setDisable(true)
          val endDateLabel = new Label("End Date:")
          val endDateInput = new DatePicker(LocalDate.now)
          endDateInput.getEditor.setDisable(true)

          // Create end time field
          val endTimeLable = new Label("End Time:")
          val endTimeInput = new TextField()
          // Set the format for the end time field
          endTimeInput.promptText = "Put time in format HH:mm:ss"

          // Create input fields for event description and category
          val descriptionLabel = new Label("Description:")
          val descriptionInput = new TextField()

          val categoryLabel = new Label("Category: COMING SOONG :D")

          // Create input fields for alarm date and time
          val alarmdateLabel = new Label("Alarm date")
          val alarmdateInput = new DatePicker(LocalDate.now)

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

          grid.add(categoryLabel,1,7)
          //grid.add(categoryInput,2,7)

          grid.add(alarmdateLabel,1,8)
          grid.add(alarmdateInput,2,8)

          grid.add(alarmtimeLabel,1,9)
          grid.add(alarmtimeInput,2,9)

          dialog.getDialogPane.setContent(grid)

          //Setting up buttons
          val submitButtonType = new ButtonType("Submit", ButtonData.OKDone)
          dialog.getDialogPane.getButtonTypes.addAll(submitButtonType,ButtonType.Cancel)
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
                descriptionInput.getText,
                alarmdateInput.getValue.toString, alarmtimeInput.getText)
            else
              null

          val result = dialog.showAndWait()

          result match {
            case Some(Result(n,s1,s2,e1,e2,d,a1,a2)) => println("name=" + n + "\nstartDate="
              + s1 + ", startTime=" + s2 + "\nendDate=" + e1 + ", endTime=" + e2 +
              "\ndescription=" + d +
              "\nalarmDate=" + a1 + "  alarmTime=" + a2)
              userInput(n,s1,s2,e1,e2,d,a1,a2)
            case Some(_) => println("ErROR :DD")
            case None => println("Dialog returned: None")
          }

        def userInput(summary: String, dateStart: String, timeStart: String, dateEnd: String, timeEnd: String, description: String, alarmDate: String, alarmTime: String) =
          //val category: Option[String] = if description.nonEmpty then Some(category) else None
          println(dateStart)

          val categories = None
          val stDate: String = dateStart.replace("-", "")
          val stTime: String = timeStart.replace(":", "")
          val stDateTime = stDate+stTime
          val endDate = dateEnd.replace("-","")
          val endTime = timeEnd.replace(":","")
          val endDateTime = endDate+endTime
          val alarDate = if alarmDate.nonEmpty then alarmDate.replace("-","")  else ""
          val alarTime = if alarmTime.nonEmpty then alarmTime.replace(":","") else ""
          val summary1 = if summary.isEmpty then None else Some(summary)
          val description1 = if description.isEmpty then None else Some(description)
          val alarmDateTIme: Option[String] = if alarDate.nonEmpty && alarTime.nonEmpty then Some(alarDate + alarTime) else None

          Events.addEventBetter(Event(generateNewUid,stDateTime,endDateTime,summary1,description1, categories, alarmDateTIme))
          Weekly_view.deleteEventsFromGrid
          Events.showEvents.foreach((i)=>Weekly_view.setEventstoGrid(i,Events.getEventEndTime(i)))

        def editEventDialog: Unit =
          val dialog = new Dialog[Unit]()
          dialog.setTitle("Edit event")
          dialog.setHeaderText("Click on the event you wish to edit and press edit Button")

          val listView = new ListView(Events.showEvents.toList)
          listView.prefHeight = 200
          listView.prefWidth = 200
          val grid = new GridPane()
          grid.add(listView,1,1)
          dialog.getDialogPane.setContent(grid)
          val deleteButton = new Button("Edit")
          grid.add(deleteButton,1,2)
          dialog.getDialogPane.getButtonTypes.addAll( ButtonType.Cancel, ButtonType.Finish)
          deleteButton.onAction = (e: ActionEvent) =>
            val selected = listView.selectionModel.apply().getSelectedItem
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
          val startDateInput = new DatePicker(Events.convertDate(key))
          val startTimeLable = new Label("Start Time:")
          val startTimeInput = new TextField()
          //startTimeInput.textFormatter = new TextFormatter(timeConverter)
          startTimeInput.setText(Events.getTime(key).format(format))
          println("Event start time" + Events.getTime(key).format(format))

          //forces user to use specific format :D
          //startTimeInput.textFormatter = new TextFormatter(timeConverter)
          startDateInput.getEditor.setDisable(true)
          val endDateLabel = new Label("End Time:")
          val endDateInput = new DatePicker(Events.convertDate(Events.getEventEndTime(key)))
          println("Event end Date:" + Events.getEventEndTime(key))
          endDateInput.getEditor.setDisable(true)

          val endTimeLable = new Label("End Time:")
          val endTimeInput = new TextField()
          //endTimeInput.textFormatter = new TextFormatter(timeConverter)
          println("Event end time:" + Events.getTime(Events.getEventEndTime(key)).format(format))

          endTimeInput.setText(Events.getTime(Events.getEventEndTime(key)).format(format))


          val descriptionLabel = new Label("Description:")
          val descriptionInput = new TextField()
          descriptionInput.setText(Events.getEventDescription(key))
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

          dialog.getDialogPane.setContent(grid)

          dialog.getDialogPane.getButtonTypes.addAll(ButtonType.Cancel)

          val updateButton = new Button("Update")
          grid.add(updateButton, 2, 7)

          val oldName = Events.getEventName(key)
          val oldStartDate = key
          val oldEndDate= Events.getEventEndTime(key)
          val oldDescription = Events.getEventDescription(key)



          updateButton.onAction = (e: ActionEvent) =>
            val newName = if nameInput.text != null then nameInput.getText else oldName
            val newStartDate = startDateInput.getValue.toString.replace("-","") + startTimeInput.getText.replace(":","")
            val newEndDate = endDateInput.getValue.toString.replace("-","") + endTimeInput.getText.replace(":","")
            val newDescription = if descriptionInput.text != null then descriptionInput.getText else oldDescription

            Events.editEventBetter(key,"startTime",Some(newStartDate))
            Events.editEventBetter(key,"endTime" ,Some(newEndDate))
            Events.editEventBetter(key,"description",Some(newDescription))
            Events.editEventBetter(key,"summary",Some(newName))
            dialog.close()




          dialog.showAndWait()
          Weekly_view.deleteEventsFromGrid
          Events.showEvents.foreach((i)=>Weekly_view.setEventstoGrid(i,Events.getEventEndTime(i)))



        def deleteDialog: Unit =
          val dialog = new Dialog[Unit]()
          dialog.setTitle("Delete event")
          dialog.setHeaderText("Click on Events you wish to delete and then press Delete Button")

          val listView = new ListView(Events.showEvents.toList)
          listView.prefHeight = 200
          listView.prefWidth = 200



          val grid = new GridPane()
          grid.add(listView,1,1)
          dialog.getDialogPane.setContent(grid)
          val deleteButton = new Button("Delete")
          grid.add(deleteButton,1,2)
          dialog.getDialogPane.getButtonTypes.addAll( ButtonType.Cancel, ButtonType.Finish)
          deleteButton.onAction = (e: ActionEvent) =>
            val selected = listView.selectionModel.apply().getSelectedItem
            if selected != null then
              Events.deleteEvent(selected.toString)
              Weekly_view.deleteEventsFromGrid
              Events.showEvents.foreach((i)=>Weekly_view.setEventstoGrid(i,Events.getEventEndTime(i)))
            listView.items = listView.items.apply().diff(Seq(selected))


          dialog.showAndWait()



       def categoriesDialog: Unit =
          val dialog = new Dialog[Unit]()
          dialog.setTitle("Categories")
          dialog.setHeaderText("Here you can edit categories and view events by categories.")
          val button = new Button("filter")
          //
          // List of all categories
          val allCategories = Events.allCategories
          val eventsGroupedByCategories = Events.groupedByCategories

          // Create checkboxes for all categories
          val checkboxes = allCategories.map((i)=>
          new CheckBox(i.get)
          ).toSeq

          button.onAction = (e: ActionEvent) =>
            handleCheckBox(checkboxes)
          val grid = new GridPane()
          for i <- checkboxes.indices do
            grid.add(checkboxes(i),i,0)
          grid.add(button,0,1)
          dialog.getDialogPane.setContent(grid)
          dialog.getDialogPane.getButtonTypes.addAll( ButtonType.Cancel, ButtonType.Finish)
          dialog.showAndWait()
          Weekly_view.deleteEventsFromGrid
          Events.showEvents.foreach((i)=>Weekly_view.setEventstoGrid(i,Events.getEventEndTime(i)))
           //Should work with this input = name,202003031700,202003031800, hopefully this works :D
       def handleCheckBox(boxes: Seq[CheckBox]) =
         var selectedList: Seq[String] = Seq[String]()
         for i <- boxes.indices do
           if boxes(i).isSelected then
             selectedList = selectedList :+ boxes(i).getText
         calendarState.appliedFilters(selectedList)



}