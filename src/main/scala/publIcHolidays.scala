import java.time.{LocalDate, LocalDateTime}

object publIcHolidays {
  val holidays: Map[String, String] = Map("0101"->"New uear's Day",
    "0601"->"Epiphany",
    "0105"->"May Day",
    "0612"->"Finland’s Independence Day",
    "2512"->"Christmas Day",
    "2612"->"Boxing Day",
    "0401"->"Test"
  )
  def findHoliday(date: String) =
    holidays.getOrElse(date,"")
    
    
}
