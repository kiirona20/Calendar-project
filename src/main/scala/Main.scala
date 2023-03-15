import scala.io.StdIn.*

object Main extends App {
    //Should work with this input = name,202003031600,202003031700, hopefully this works :D

  println("Choose to add an event, edit event or delete an event")
  var userInput = readLine()
  if userInput.toLowerCase == "add" then
    println("Write the event name, date, starTime, endTime and description. Seperate these by comma ','")
    userInput = readLine()

  if userInput.toLowerCase == "edit" then
    println("Which event do you wish to edit\nList of recorded events: " + Events(userInput).readFile.keys)
    userInput = readLine()

  if userInput.toLowerCase == "delete" then
    println("Which event do you wish to delete\nList of recorded events: " + Events(userInput).readFile.keys)
    userInput = readLine()











}
