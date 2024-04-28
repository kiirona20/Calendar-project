
object calendarState {
  var appliedFilter = Seq[String]()
  var appliedFilter2 = Seq[String]()

  def appliedFilters(filters: Seq[String]): Unit =
    appliedFilter = filters

  def appliedFiltersDialog(filters: Seq[String]): Unit =
    appliedFilter2 = filters
    
}
