import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.time.format.DateTimeFormatter

object dateTimeHandler {
  
  private val dateFormat = DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
  private val dateFormat2 = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")
  private val dayMonthFormat = DateTimeFormatter.ofPattern("MMdd")

  def getdayOfWeek(date: String): Int =
    LocalDateTime.parse(date,dateFormat).getDayOfWeek.getValue

  def getHour(date: String): Int =
    LocalDateTime.parse(date, dateFormat).getHour

  def getMin(date: String): Int =
    LocalDateTime.parse(date, dateFormat).getMinute

  def convertStringToTime(date: String): LocalTime =
    LocalTime.parse(date, dateFormat)  
  
  def convertStringToDate(date: String): LocalDate =
    LocalDateTime.parse(date, dateFormat).toLocalDate

  def convertStringToDateTIme(date: String): LocalDateTime =
    LocalDateTime.parse(date, dateFormat)


  def getDateOnly(date:LocalDate): String = date.format(dayMonthFormat)
  
  def convertLocalDateTimeToFormat(date: LocalDateTime): String = date.format(dateFormat)

 

}
