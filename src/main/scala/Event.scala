
class Event(var uid:String = "", var startTime: String = "",
            var endTime: String = "", var summary: Option[String] = None,
            var description: Option[String] = None, var categories: Option[String] = None,
            var trigger: Option[String] = None){


  def findAndEdit(element: String, change: Option[String]): Unit = 
    val check = change.isDefined
    element match
      case "startTime" => if check then startTime = change.get 
      case "endTime" => if check then endTime = change.get 
      case "summary" => summary = change
      case "description" => description = change
      case "categories" => categories = change
      case "trigger" => trigger = change
      case _ => println("Element not found or cannot be changed")


}

