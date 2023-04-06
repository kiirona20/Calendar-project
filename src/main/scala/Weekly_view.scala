import scalafx.Includes._
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.control._
import scalafx.event.ActionEvent


object Weekly_view extends JFXApp3:

  def start(): Unit =

   /*
    Creation of a new primary stage (Application window).
    We can use Scala's anonymous subclass syntax to get quite
    readable code.
    */


    stage = new JFXApp3.PrimaryStage{
      title = "jee"
      scene = new Scene(600,400){
        val button = new Button("Delete event :D")
        button.layoutX = 200
        button.layoutY = 50

        val textField = new TextField
        textField.layoutY = 300
        textField.layoutX = 300
        textField.promptText = "add event here"


        //shows all the Events :)
        val listView = new ListView(Events.showEvents.toList)
        listView.prefHeight = 200
        listView.prefWidth = 200


        //content shows everything that is on the screen
        content = List(button,listView,textField)
        //Deletes an item from the listview and from eventINfo when you press a button :D
        button.onAction = (e:ActionEvent) => {
          val selected = listView.selectionModel.apply().getSelectedItems
          if selected.isDefinedAt(0) then Events.deleteEvent(selected.get(0))
          listView.items = listView.items.apply().diff(selected)

          println("Button clicked")
        }
      }
    }