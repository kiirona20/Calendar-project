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
import scalafx.scene.layout.GridPane.getHgrow
import scalafx.scene.input.*

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

   /*
    Creation of a new primary stage (Application window).
    We can use Scala's anonymous subclass syntax to get quite
    readable code.
    */


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
        //date.onMouseClicked = (me: MouseEvent) => {
          //if me.button == MouseButton.Primary then
              //val picker = new DatePicker()
              //val popup = new Popup{
              //val button = new Button("close")
              //content.addAll(picker,button)
              //}
             // button.onAction = (e: ActionEvent) => {
               // popup.hide()
              //  println("buttonclikced")}



              //popup.show(this.window(),me.screenX,me.screenY)

              //println("Lol")


        //}




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
          Events.showEvents.foreach(setEventstoGrid(_))

        }
        button2.onAction = (e: ActionEvent) => {
          dateTracker = dateTracker.plusWeeks(1)
          date.text = dateTracker.toString
          deleteEventsFromGrid
          Events.showEvents.foreach(setEventstoGrid(_))


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
        def setEventstoGrid(date: String) =
          val convertedDate = Events.convertDate(date)


          val weekDay = dateTracker.getDayOfWeek.getValue
          var startOfTheWeekDate = dateTracker.minusDays(weekDay - 1)
          //Checks if the 'date' is in the current week.
          //If it is then it will added to the grid and to the Event List,
          for i <- 0 until(7) do
            if startOfTheWeekDate == convertedDate then
              val x = Events.getdayOfWeek(date)
              val y = Events.getTime(date)
              val label = new Label(Events.getEventName(date))
              allEventChildren = allEventChildren.appended(label)
              println(allEventChildren)
              gridpane.add(label,x,y+1)
            startOfTheWeekDate = startOfTheWeekDate.plusDays(1)




        //Deletes all Events that are in the grid
        def deleteEventsFromGrid: Unit =
          allEventChildren.foreach((i)=> gridpane.children -= i)
          allEventChildren = allEventChildren.empty


        //Sets all events to the grid
        Events.showEvents.foreach(setEventstoGrid(_))




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


        //val contextMenu = new ContextMenu(new MenuItem("add"), new MenuItem("edit"), new MenuItem("delete"))
        //label.contextMenu = contextMenu
        //content shows everything that is on the screen
        //content = List(label)
        //Deletes an item from the listview and from eventINfo when you press a button :D
        //button.onAction = (e:ActionEvent) => {
          //val selected = listView.selectionModel.apply().getSelectedItems
          //if selected.isDefinedAt(0) then Events.deleteEvent(selected.get(0))
          //listView.items = listView.items.apply().diff(selected)

          //println("Button clicked")
        //}


      }
    }