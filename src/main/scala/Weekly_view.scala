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


  // Define a start function to set up a weekly calendar

  def start(): Unit =
    // Define the days of the week
    val days = List[String]("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    // Variables for tracking the current date and events
    var dateTracker = Events.getDateToday
    var allEventChildren = List[Node]()


    // Variables for scene size
    var sceneWidth = 800.0
    var sceneHeight = 800.0
    // Define the stage
    stage = new JFXApp3.PrimaryStage{
      title = "Calendar"
      scene = new Scene(sceneWidth,sceneHeight){


        //Creates a gridpane layout.
        val gridpane = new GridPane
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


        //add labels for each day of the week
        for i <- days.indices do
          val label = new Label(days(i))
          gridpane.add(label,i+1,0)

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

        //Deletes all Events that are in the grid
        def deleteEventsFromGrid: Unit =
          // Remove all events from the grid
          allEventChildren.foreach((i)=> gridpane.children -= i)
          // Empty the list of all events
          allEventChildren = allEventChildren.empty

        // Add all events to the grid
        Events.showEvents.foreach((i)=>setEventstoGrid(i,Events.getEventEndTime(i)))

        root = gridpane

        // Function to display a dialog for adding a new event
        def EventInputDialog(): Unit =
          // Create a new dialog for adding events
          val dialog = new Dialog[(String, String, String, String, String, String, String, String, String)]()
          dialog.setTitle("Add Event")
          dialog.setHeaderText("Enter event details")

          // Define the format for the time fields
          val timePattern = "HH:mm:ss"
          val format = DateTimeFormatter.ofPattern(timePattern)
          val timeConverter = new LocalTimeStringConverter(format, format)

          // Create input fields for event details
          val nameLabel = new Label("Event Name:")
          val nameInput = new TextField()
          val startDateLabel = new Label("Start Date:")
          val startDateInput = new DatePicker()
          val startTimeLable = new Label("Start Time:")
          val startTimeInput = new TextField()
          // Set the format for the start time field
          startTimeInput.promptText = "Put time in format HH:mm:ss"
          startTimeInput.textFormatter = new TextFormatter(timeConverter)
          // Disable editing of the date fields
          startDateInput.getEditor.disable = true
          val endDateLabel = new Label("End Date:")
          val endDateInput = new DatePicker()
          endDateInput.getEditor.disable = true

          // Create end time field
          val endTimeLable = new Label("End Time:")
          val endTimeInput = new TextField()
          // Set the format for the end time field
          endTimeInput.promptText = "Put time in format HH:mm:ss"
          endTimeInput.textFormatter = new TextFormatter(timeConverter)

          // Create input fields for event description and category
          val descriptionLabel = new Label("Description:")
          val descriptionInput = new TextField()

          val categoryLabel = new Label("Category:")
          val categoryInput = new ComboBox[String](FXCollections.observableArrayList(AppState.allCategories: _*))

          // Create input fields for alarm date and time
          val alarmdateLabel = new Label("Alarm date")
          val alarmdateInput = new DatePicker()

          val alarmtimeLabel = new Label("Alarm time")
          val alarmtimeInput = new TextField()

          alarmtimeInput.textFormatter = new TextFormatter(timeConverter)

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
          grid.add(categoryInput,2,7)

          grid.add(alarmdateLabel,1,8)
          grid.add(alarmdateInput,2,8)

          grid.add(alarmtimeLabel,1,9)
          grid.add(alarmtimeInput,2,9)

          dialog.getDialogPane.setContent(grid)

          val submitButtonType = new ButtonType("Submit", ButtonData.OKDone)
          dialog.getDialogPane.getButtonTypes.addAll(submitButtonType, ButtonType.Cancel)

          // Disables the submit button if any of the fields are empty.
          // '<==' Binds the right side to the left side so that whenever the value is true from right it disbles the button
          dialog.getDialogPane.lookupButton(submitButtonType).disable <== nameInput.text.isEmpty || descriptionInput.text.isEmpty || startDateInput.getEditor.text.isEmpty || endDateInput.getEditor.text.isEmpty
            || startTimeInput.text.isEmpty || endTimeInput.text.isEmpty



          dialog.resultConverter = button =>
            if button == submitButtonType then
              ( nameInput.getText,
              if startDateInput.getValue != null then startDateInput.getValue.toString else "",
              startTimeInput.getText,
              if endDateInput.getValue != null then endDateInput.getValue.toString else "",
              endTimeInput.getText,
              descriptionInput.getText,
              if alarmdateInput.getValue != null then alarmdateInput.getValue.toString else "",
              alarmtimeInput.getText,
              categoryInput.getSelectionModel.getSelectedItem)
            else
              null

          val result = dialog.showAndWait()

          if result.isDefined then
            userInput(nameInput.getText,
            if (startDateInput.getValue != null) startDateInput.getValue.toString else "",
            startTimeInput.getText,
            if (endDateInput.getValue != null) endDateInput.getValue.toString else "",
            endTimeInput.getText,
            descriptionInput.getText,
            if (alarmdateInput.getValue != null) alarmdateInput.getValue.toString else "",
            alarmtimeInput.getText,
            categoryInput.getSelectionModel.getSelectedItem)

          val eventName = nameInput.getText

           //Should work with this input = name,202003031700,202003031800, hopefully this works :D
        def userInput(name: String, dateStart: String, timeStart: String, dateEnd: String, timeEnd: String, description: String, alarmDate: String, alarmTime: String, category: String) =
          val stDate: String = dateStart.replace("-", "")
          val stTime: String = timeStart.replace(":", "")
          val stDateTime = stDate+stTime
          val endDate = dateEnd.replace("-","")
          val endTime = timeEnd.replace(":","")
          val endDateTime = endDate+endTime
          val alarDate = if alarmDate != "" then alarmDate.replace("-","")  else ""
          val alarTime = if alarmTime != "" then alarmTime.replace(":","") else ""

          val alarmDateTIme = if alarDate != "" && alarTime != "" then alarDate + alarTime else ""
          val categ = if category != "" then category else "None"
          //categ, alarmdatetime are optional
          Events.addEvent(stDateTime + "," + endDateTime + "," + name + "," + description + "," + categ + "," + alarmDateTIme)
          deleteEventsFromGrid
          Events.showEvents.foreach((i)=>setEventstoGrid(i,Events.getEventEndTime(i)))
          if(alarmDateTIme != ""){
            val alarmDateTimeObj = LocalDateTime.parse(alarmDateTIme.slice(0, 8) + "T" + alarmDateTIme.drop(8) + "Z", DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'"))
            val reminderTime = Duration.between(LocalDateTime.now(), alarmDateTimeObj)
            reminderActor ! Reminder(name, reminderTime)

          }



        val contextmenu = new ContextMenu()
        contextmenu.items.add((new MenuItem("Add"){onAction = () => Dialogs.EventInputDialog()}))
        contextmenu.items.add((new MenuItem("Edit"){onAction = () => editEventDialog}))
        contextmenu.items.add((new MenuItem("Delete"){onAction = () => deleteDialog}))
        contextmenu.items.add(new MenuItem("Categories"){onAction = () => categoriesDialog})
        contextmenu.items.add(new MenuItem("add Categirues"){onAction = () => addCategoryDialog})




        def categoriesDialog: Unit =
          val dialog = new Dialog[Unit]()
          dialog.setTitle("Categories")
          dialog.setHeaderText("Here you can edit categories and view events by categories.")

          // List of all categories
          val allCategories = Events.allCategories
          val eventsGroupedByCategories = Events.groupedByCategories

          // Create checkboxes for all categories
          val checkboxes = allCategories.map { category =>
            val checkbox = new CheckBox(category)
            checkbox.selected = AppState.selectedCategories.contains(category)
            checkbox
          }

          // Create a ComboBox for category selection
          val categoryComboBox = new ComboBox[String](FXCollections.observableArrayList(AppState.allCategories: _*))

          val grid = new GridPane()

          checkboxes.zipWithIndex.foreach { case (checkbox, i) =>
            grid.add(checkbox, 1, i + 1)}


          // Add the ComboBox to your grid
          grid.add(categoryComboBox, 1, checkboxes.size + 1)

          val deleteButton = new Button("Delete Category")
          deleteButton.setOnAction((e: ActionEvent) => {
            val selectedCategory = categoryComboBox.getSelectionModel.getSelectedItem
            if (selectedCategory != null) {
              AppState.deleteCategory(selectedCategory)

              Events.groupedByCategories(selectedCategory).foreach((i)=>Events.deleteEvent(i._1))
              Events.showEvents.foreach((i)=>setEventstoGrid(i,Events.getEventEndTime(i)))            }
          })

          // Add the delete button to your grid
          grid.add(deleteButton, 1, checkboxes.size + 2)

          dialog.getDialogPane.setContent(grid)

          val submitButtonType = new ButtonType("Submit", ButtonData.OKDone)
          dialog.getDialogPane.getButtonTypes.addAll(submitButtonType, ButtonType.Cancel)

          dialog.resultConverter = button => {
            if (button == submitButtonType) {
              // Filter events by the selected categories and set them to the grid
              AppState.selectedCategories = checkboxes.filter(_.selected.value).map(_.text.value).toList
              applyCategoryFilter()
            }
          }

          dialog.showAndWait()
        def applyCategoryFilter(): Unit =
            val eventsGroupedByCategories = Events.groupedByCategories
            deleteEventsFromGrid
            AppState.selectedCategories.foreach { category =>
                eventsGroupedByCategories(category).foreach(i => setEventstoGrid(i._1, Events.getEventEndTime(i._1)))
            }

        onMouseClicked = (me: MouseEvent) =>
          if me.button == MouseButton.Secondary then
            contextmenu.show(this.window(),me.screenX,me.screenY)

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
              deleteEventsFromGrid
              Events.showEvents.foreach((i)=>setEventstoGrid(i,Events.getEventEndTime(i)))
            listView.items = listView.items.apply().diff(Seq(selected))


          dialog.showAndWait()
        // an object that keeps track of App state. By default every Category is selected.
        private val basiccategories = List[String]("Study","Hobbies", "Work")
        object AppState {
          var allCategories: List[String] = Events.allCategories.toList ++ basiccategories
          var selectedCategories: List[String] = allCategories

          def deleteCategory(category: String): Unit =
            allCategories = allCategories.filterNot(_ == category)
            selectedCategories = selectedCategories.filterNot(_ == category)
        }

        def addCategoryDialog: Unit =
          val dialog = new Dialog[String]()
          dialog.setTitle("New Category")
          dialog.setHeaderText("Enter new category details.")

          val nameLabel = new Label("Category Name:")
          val nameInput = new TextField()

          val grid = new GridPane()
          grid.add(nameLabel, 1, 1)
          grid.add(nameInput, 2, 1)

          dialog.getDialogPane.setContent(grid)

          val submitButtonType = new ButtonType("Submit", ButtonData.OKDone)
          dialog.getDialogPane.getButtonTypes.addAll(submitButtonType, ButtonType.Cancel)

          dialog.resultConverter = button =>
            if (button == submitButtonType)
              nameInput.getText
            else
              null

          val result = dialog.showAndWait()

          result.foreach { category =>
            AppState.allCategories = category.toString :: AppState.allCategories
          }


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
          val timeConverter = new LocalTimeStringConverter(format, format)


          val nameLabel = new Label("Event Name:")
          val nameInput = new TextField()
          nameInput.setText(Events.getEventName(key))
          val startDateLabel = new Label("Start Date:")
          val startDateInput = new DatePicker(Events.convertDate(key))
          val startTimeLable = new Label("Start Time:")
          val startTimeInput = new TextField()
          startTimeInput.textFormatter = new TextFormatter(timeConverter)
          startTimeInput.setText(Events.getTime(key).toString)

          //forces user to use specific format :D
          //startTimeInput.textFormatter = new TextFormatter(timeConverter)
          startDateInput.getEditor.disable = true
          val endDateLabel = new Label("End Time:")
          val endDateInput = new DatePicker(Events.convertDate(Events.getEventEndTime(key)))
          endDateInput.getEditor.disable = true

          val endTimeLable = new Label("End Time:")
          val endTimeInput = new TextField()
          endTimeInput.textFormatter = new TextFormatter(timeConverter)

          endTimeInput.setText(Events.getTime(Events.getEventEndTime(key)).toString)


          val descriptionLabel = new Label("Description:")
          val descriptionInput = new TextField()
          descriptionInput.setText(Events.getEventDescription(key))

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
          println(oldEndDate)
          val oldDescription = Events.getEventDescription(key)



          updateButton.onAction = (e: ActionEvent) =>
            val newName = if nameInput.text != null then nameInput.getText else oldName
            val newStartDate = startDateInput.getValue.toString.replace("-","") + startTimeInput.getText.replace(":","")
            val newEndDate = endDateInput.getValue.toString.replace("-","") + endTimeInput.getText.replace(":","")
            val newDescription = if descriptionInput.text != null then descriptionInput.getText else oldDescription

            Events.editEvent(key,(oldStartDate,newStartDate))
            Events.editEvent(key,(oldEndDate,newEndDate))
            Events.editEvent(key,(oldDescription,newDescription))
            Events.editEvent(key,(oldName,newName))
            deleteEventsFromGrid
            Events.showEvents.foreach((i)=>setEventstoGrid(i,Events.getEventEndTime(i)))
            dialog.close()



          dialog.showAndWait()






      }
    }
