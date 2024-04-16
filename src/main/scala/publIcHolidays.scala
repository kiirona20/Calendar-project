import java.time.{LocalDate, LocalDateTime}

object publIcHolidays {
  val holidays: Map[String, String] = Map("0101"->"New uear's Day",
    "0601"->"Epiphany",
    "0501"->"May Day",
    "1206"->"Finland’s Independence Day",
    "1225"->"Christmas Day",
    "1226"->"Boxing Day",
    "0401"->"Test",
    "0414"->"Test"
  )
  def findHoliday(date: String) =
    holidays.getOrElse(date,"")
    
    
}
