import scala.io.StdIn.*

object Main extends App {
  println("Write the event name, date, starTime, endTime and description. Seperate these by comma ','")
  //Should work with this input = name,202003031600,202003031700, hopefully this works :D
  var userInput = readLine()
  Events(userInput).writetoFile





}
