import javafx.geometry.VPos
import javafx.scene.layout.ColumnConstraints
import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.Node
import scalafx.scene.layout.*
import scalafx.scene.control.*
import scalafx.scene.layout.GridPane
import scalafx.event.ActionEvent
import scalafx.geometry.HPos
import scalafx.scene.layout.GridPane.{getHgrow, getRowIndex, getValignment, setHalignment, setRowSpan, setValignment}
import scalafx.scene.input.*
import scalafx.scene.paint.Color

import java.util.Date.*
import scalafx.stage.Popup

import java.awt.Insets
import java.time.temporal.TemporalQueries.localDate
import scala.annotation.internal.Child


object Weekly_view extends JFXApp3:







  def start(): Unit =

    val days = List[String]("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    var dateTracker = Events.getDateToday
    var allEventChildren = List[Node]()

  
    stage = new JFXApp3.PrimaryStage{
      title = "Calendar"
      scene = new Scene(800,800){

        //Creates a gridpane layout.
        val gridpane = new GridPane
        //for debuggin purposes
        gridpane.gridLinesVisible = true
        //sets gaps between the "days"
        gridpane.setHgap(10)


        //Column width persentage set to 100/8 (7 days + time column + buttons)
        val columnWidthPercentage = 100 / 10
        for i <- 0 until(10) do
          val column = new ColumnConstraints()
          column.percentWidth = columnWidthPercentage
          gridpane.getColumnConstraints().add(column)
        //Time label set to column 0 and to row 1
        //val time = new Label("Time")

        var date = new Label(dateTracker.toString)
        gridpane.add(date,0,0)
   

        //Days labels set to columns 1-7 and to row 1.
        for i <- 0until(days.length) do
          val label = new Label(days(i))
          gridpane.add(label,i+1,0)

        //Created Buttons to move between weeks
        val button1 = new Button("<--")
        val button2 = new Button("-->")
        gridpane.add(button1,8,0)
        gridpane.add(button2,9,0)

        //Button1 Moves backwards and button2 moves forward a week
        //First they remove Events from the grid.
        //Then they add Events that are in the week.
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

        var clock = 0.00
        //Time slots will be set to 60 mins. So rowsHeightPercetage will be 100 / (24)
        val rowsHeightPercentage = 100 / 24

        for i <- 0 until(24) do
          val row = new RowConstraints()
          row.percentHeight = rowsHeightPercentage
          gridpane.getRowConstraints.add(row)
          val label = new Label(clock.toString)
          gridpane.add(label,0,i+1)
          clock += 1


        //Helper function for setting things to the grid
        def setEventstoGrid(dateStart: String, dateEnd: String) =
          val convertedDateStart = Events.convertDate(dateStart)
          val convertedDateEnd = Events.convertDate(dateEnd)


          val weekDay = dateTracker.getDayOfWeek.getValue
          var startOfTheWeekDate = dateTracker.minusDays(weekDay - 1)
          val background = Background
          //Checks if the 'date' is in the current week.
          //If it is then it will added to the grid and to the Event List,
          for i <- 0 until(7) do
            if startOfTheWeekDate == convertedDateStart then
              val x = Events.getdayOfWeek(dateStart)
              val y = Events.getTime(dateStart)
              val x_2 = Events.getdayOfWeek(dateEnd)
              val y_2 = Events.getTime(dateEnd)
              val textArea = new TextArea
              
              //Disables editing capabilities and makes mouse transparent when howering on it
              textArea.editable = false
              textArea.mouseTransparent = true
              textArea.setFocusTraversable(false)
              textArea.text = (Events.getEventName(dateStart))


              allEventChildren = allEventChildren.appended(textArea)
              println(allEventChildren)
              setRowSpan(textArea, y_2-y)
              //sets backgrounf coreners to green
              textArea.setBackground(background.fill(Color.Green))
              //sets innerbackgrounf to green using css
              
              textArea.setStyle("-fx-control-inner-background: green")

              gridpane.add(textArea,x,y+1)
              
            startOfTheWeekDate = startOfTheWeekDate.plusDays(1)


        //Deletes all Events that are in the grid
        def deleteEventsFromGrid: Unit =
          allEventChildren.foreach((i)=> gridpane.children -= i)
          allEventChildren = allEventChildren.empty

        //Sets all events to the grid
        Events.showEvents.foreach((i)=>setEventstoGrid(i,Events.getEventEndTime(i)))

        root = gridpane

        val contextmenu = new ContextMenu(new MenuItem("Add"), new MenuItem("Edit"), new MenuItem("Delete"))

        // On right click creates a contextMenu anywhere
        onMouseClicked = (me: MouseEvent) => {
          if me.button == MouseButton.Secondary then
            contextmenu.show(this.window(),me.screenX,me.screenY)          }


        //shows all the Events :)
        val listView = new ListView(Events.showEvents.toList)
        listView.prefHeight = 200
        listView.prefWidth = 200


      }
    }