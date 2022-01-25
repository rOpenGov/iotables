old_table_selection_function <- function(labelled_io_data, year_input, geo_input, unit_input, stk_flow_input) {
  ## This internal function used to be a part of iotable_get() and it is no longer in use.
  ## It is replaced by get_saved_table().
  
  selected_table <- which (   ## get the number of table to be selected
    labelled_io_data$year == year_input & 
      as.character(labelled_io_data$geo) == geo_input &
      labelled_io_data$unit == unit_input )
  
  if ( length( selected_table) == 0  )  {
    stop ( paste0("There is no available table for country ", geo_input, 
                  " in the year ", year_input, 
                  " with ", unit_input, " units.") )
  } else if (length( selected_table) == 3) { 
    selected_table <- which (   ##get the number of table to be selected
      labelled_io_data$year == year_input & 
        as.character(labelled_io_data$geo) == geo_input &
        labelled_io_data$unit == unit_input  &
        labelled_io_data$stk_flow == stk_flow_input
    )
  }  
  
  if (length(selected_table) != 1) {
    stop ( "The parameters geo=", geo, "; unit=",  unit_input, 
           "; stk_flow=", stk_flow_input, 
           "\ndo not select a unique table.")
  }
  
  iotable <- labelled_io_data$data[[selected_table]]
}
