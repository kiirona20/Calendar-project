
object calendarState {
  var allFilters = Seq[String]("Conference", "Meetings", "Something else")
  var appliedFilter = Seq[String]()
  var appliedFilter2 = Seq[String]()

  
  def addFilter(filter: String) =
    allFilters = allFilters :+ filter
  
  def appliedFilters(filters: Seq[String]) =
    appliedFilter = filters
    
  def appliedFiltersDialog(filters: Seq[String]) =
    appliedFilter2 = filters  
    
}
