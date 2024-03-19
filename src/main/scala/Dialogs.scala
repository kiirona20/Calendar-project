import Main.userInput
import scalafx.Includes.{jfxBooleanBinding2sfx, jfxBooleanProperty2sfx}
import scalafx.beans.property.BooleanProperty

import scala.compiletime.ops.boolean.||
import javafx.util.converter.LocalTimeStringConverter
import scalafx.scene.control.{Alert, ButtonType, ComboBox, DatePicker, Dialog, Label, TextField, TextFormatter}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.layout.GridPane

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDateTime}

object Dialogs {
  //Result class to store are dialog values
  case class Result(name: String, startDate: String, startTime: String, endDate: String, endTime: String, description: String, alarmDate: String, alarmTime: String)

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
          val startDateInput = new DatePicker()
          val startTimeLable = new Label("Start Time:")
          val startTimeInput = new TextField()
          // Set the format for the start time field
          startTimeInput.promptText = "Put time in format HH:mm:ss"

          // Disable editing of the date fields
          startDateInput.getEditor.setDisable(true)
          val endDateLabel = new Label("End Date:")
          val endDateInput = new DatePicker()
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
          submitButton.disableProperty() <== nameInput.text.isEmpty || descriptionInput.text.isEmpty || startDateInput.getEditor.textProperty.isEmpty || endDateInput.getEditor.textProperty.isEmpty
           || startTimeInput.text.isEmpty || endTimeInput.text.isEmpty


          dialog.resultConverter = button =>
            if button == submitButtonType then
              Result(nameInput.getText,
                startDateInput.getEditor.getText, startTimeInput.getText,
                endDateInput.getEditor.getText, endTimeInput.getText,
                descriptionInput.getText,
                alarmdateInput.getEditor.getText, alarmtimeInput.getText)
            else
              null

          val result = dialog.showAndWait()

          result match {
            case Some(Result(n,s1,s2,e1,e2,d,a1,a2)) => println("name=" + n + "\nstartDate="
              + s1 + ", startTime=" + s2 + "\nendDate=" + e1 + ", endTime=" + e2 +
              "\ndescription=" + d +
              "\nalarmDate=" + a1 + "  alarmTime=" + a2)
            case None => println("Dialog returned: None")
          }
          val eventName = nameInput.getText //mikä tää on

          
           //Should work with this input = name,202003031700,202003031800, hopefully this works :D
}