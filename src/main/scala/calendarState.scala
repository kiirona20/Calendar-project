
object calendarState {
  var appliedFilter = Seq[String]()
  var appliedFilter2 = Seq[String]()

  def appliedFilters(filters: Seq[String]) =
    appliedFilter = filters

  def appliedFiltersDialog(filters: Seq[String]) =
    appliedFilter2 = filters
    
}
