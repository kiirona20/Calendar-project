import java.util.StringTokenizer

object calendarState {
  var allFilters = Seq[String]("Conference", "Meetings", "Something else")
  var appliedFilter = Seq[String]()
  
  def addFilter(filter: String) =
    allFilters = allFilters :+ filter
  
  def appliedFilters(filters: Seq[String]) =
    appliedFilter = filters
}
