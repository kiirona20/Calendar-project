import javafx.scene.layout.ColumnConstraints
import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.layout.*
import scalafx.scene.control.*
import scalafx.scene.layout.GridPane
import scalafx.event.ActionEvent
import scalafx.scene.layout.GridPane.getHgrow

import java.awt.Insets
import scala.collection.immutable.BitSet.empty.until


object Weekly_view extends JFXApp3:

  def start(): Unit =

    val days = List[String]("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
   /*
    Creation of a new primary stage (Application window).
    We can use Scala's anonymous subclass syntax to get quite
    readable code.
    */


    stage = new JFXApp3.PrimaryStage{
      title = "Calendar"
      scene = new Scene(600,400){
        val button = new Button("Delete event :D")
        button.layoutX = 200
        button.layoutY = 50
        //Creates a gridpane layout.
        val gridpane = new GridPane
        //for debuggin purposes
        gridpane.gridLinesVisible = true
        //sets gaps between the "days"
        gridpane.setHgap(10)


        //Column width persentage set to 100/8 (7 days + time column)
        val columnWidthPercentage = 100 / 8
        for i <- 0 until(8) do
          val column = new ColumnConstraints()
          column.percentWidth = columnWidthPercentage
          gridpane.getColumnConstraints().add(column)
        //Time label set to column 0 and to row 1
        val time = new Label("Time")
        gridpane.add(time,0,0)
        //Days labels set to columns 1-7 and to row 1.
        for i <- 0until(days.length) do
          val label = new Label(days(i))
          gridpane.add(label,i+1,0)

        var clock = 0.00
        //Time slots will be set to 30 mins. So rowsHeightPercetage will be 100 / (24/0.5)
        val rowsHeightPercentage = 100 / 48

        for i <- 0 until(48) do
          val row = new RowConstraints()
          row.percentHeight = rowsHeightPercentage
          gridpane.getRowConstraints.add(row)
          val label = new Label(clock.toString)
          gridpane.add(label,0,i+1)
          clock += 0.5






        root = gridpane





        //shows all the Events :)
        val listView = new ListView(Events.showEvents.toList)
        listView.prefHeight = 200
        listView.prefWidth = 200

        //creates leabal height and width
        //val label = new Label("Monday")
        //label.layoutY = 0
        //label.layoutX = 0
        //label.prefWidth = 200
        //label.prefHeight = 400



        val contextMenu = new ContextMenu(new MenuItem("add"), new MenuItem("edit"), new MenuItem("delete"))
        //label.contextMenu = contextMenu
        //content shows everything that is on the screen
        //content = List(label)
        //Deletes an item from the listview and from eventINfo when you press a button :D
        button.onAction = (e:ActionEvent) => {
          val selected = listView.selectionModel.apply().getSelectedItems
          if selected.isDefinedAt(0) then Events.deleteEvent(selected.get(0))
          listView.items = listView.items.apply().diff(selected)

          println("Button clicked")
        }


      }
    }