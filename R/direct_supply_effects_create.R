#' Create direct effects on total supply
#' 
#' The function creates the direct effects on total supply (or other supply 
#' category). See Eurostat Manual p498.
#' @param labelled_io_table A named (primary) input(s) vector or matrix created by \code{\link{primary_input_get}}
#' @param type Defaults to \code{final_demand}. Alternatives are \code{domestic_demand} or 
#' or \code{'intermediate_demand'}.
#' @param digits Rounding digits, defaults to \code{NULL}, in which case 
#' no rounding takes place.  
#' @importFrom dplyr select 
#' @examples  
#' 
#' io_table <- iotable_get () 
#' #Total column should not be missing
#' io_table <- io_table [, 1:7] 
#' io_table$total <- rowSums(io_table[, 2:7])
#' 
#' labelled_io_table <- io_table
#' direct_supply_effects_create ( io_table ) 
#' @export

direct_supply_effects_create <- function ( labelled_io_table,
                                    type = 'final_demand',
                                    digits = NULL ) { 
  if (! is.null(digits)) {
    if (digits<0) digits <- NULL
  }
  
  
  ###Removing all zero columns and rows --------
  
  #Determine if a column is all zero
  non_zero <- function (x) {
    if ( class ( x ) %in% c("factor", "character") ) return ( TRUE )
    ifelse (  sum ( as.numeric ( unlist (x) ), na.rm=TRUE) == 0, FALSE, TRUE )
  }
  
  #Examine which columns are filled with zeros
  non_zero_cols <- vapply ( labelled_io_table, 
                            non_zero, logical (1) )
  non_zero_rows <- labelled_io_table[-1] 
  
  #Remove columns that are filled with zeros
  remove_cols <- names (labelled_io_table)[! non_zero_cols ]
  
  if ( length( remove_cols) > 0 ) {
    warning ("Columns ", paste(remove_cols, collapse =', '), " are all zeros and will be removed.")
  }
  
  siot_rows <- as.character ( unlist ( labelled_io_table[,1]) )
  
  ##Now remove all zero corresponding rows
  labelled_io_table<- labelled_io_table [! siot_rows %in% remove_cols , 
                                         ! names ( labelled_io_table) %in% remove_cols  ]
  
  labelled_io_table <- dplyr::mutate_if ( labelled_io_table, 
                                          is.factor, as.character )
  
  ###Separate io table quadrants------
  total_row_number <- which (tolower(as.character(unlist(labelled_io_table[,1]))) %in% c("total", "cpa_total"))
  total_column_number <- which ( tolower(names(labelled_io_table)) %in% c("cpa_total", "total"))
  
  
  lq_rows <- total_row_number:nrow(labelled_io_table)
  lower_quadrant <- labelled_io_table [ lq_rows, 1:ncol(labelled_io_table) ]
  
  if ( (any(c(nrow(lower_quadrant), ncol(lower_quadrant))) == 0 ) ) {
    stop ( "No lower quadrant was selected.")
  }
  
  ##Remove fully missing rows------
  fully_missing_rows <- which ( rowSums(is.na(lower_quadrant))
                                == ncol(lower_quadrant))
  if ( length( fully_missing_rows) >0 ) { 
    lower_quadrant <- lower_quadrant[-fully_missing_rows, ]
    warning("Fully missing rows were removed from the table.")
    }

   ###Which type of comparions?-------
    if (type == "intermediate") {
    demand_row_number <- which (tolower(as.character(unlist(labelled_io_table[,1]))) %in% c("total", "cpa_total"))
  } else if (type == "domestic_demand")  {
    demand_row_number <- which ( tolower(
      as.character(unlist(labelled_io_table[,1]))) %in% c(
        "output", "p1","intermediate_consumption_pp"))
  } else if (type == "final_demand")  {
    demand_row_number <- which ( tolower(
      as.character(unlist(labelled_io_table[,1]))) %in% c(
        "ts_bp", "output_bp", "total_supply_bp"))
  } else {
    stop("Type must be any of 'intermediate' or 'domestic_demand' or 'final_demand'.")
  }
  
  if ( length (demand_row_number) == 0 ) {
    stop("Demand row was not found.")
  }
  
  ###Select demand row-----
  demand_row <- labelled_io_table[demand_row_number, 1:total_column_number ]
  not_na_cols <- which (! is.na(as.numeric(demand_row[ 2:length(demand_row)])))
  
  if (any(! not_na_cols)) {
    warning ( "The columns ", 
              paste(names(labelled_io_table)[!not_na_cols], collapse = ', '), 
              "were removed due to missing demand data.")
  }
  
  #remove columns where demand is not known
  demand_row <- demand_row [, not_na_cols]
  demand_row
  
  lower_quadrant <- lower_quadrant [, not_na_cols]
  
  zero_totals <- which ( unlist(demand_row[, c(2:length(demand_row))]) == 0  )
  
  if ( length(zero_totals > 0) ) {
    for ( j in zero_totals ) {
      demand_row[, j] <- 0.0000001
      warning ( "Zero total value ", 
                names ( demand_row )[j], " was changed to to epsilon do avoid division by zero")
    }
  } #end of replacing zeros 
  
  for ( j in 1:nrow(lower_quadrant)) {
    lower_quadrant[j, 2:ncol(lower_quadrant)] <- as.numeric(
      lower_quadrant[j,2:ncol(lower_quadrant)])/as.numeric(demand_row[,2:ncol(demand_row)])
  }
  
  if ( ! is.null(digits)) {
    if (! class(digits) %in% c("numeric", "integer")) {
      stop("Digits must be a number.")
    }
    lower_quadrant[, 2:ncol(lower_quadrant)] <- round(lower_quadrant[, 2:ncol(lower_quadrant)], digits) #actual round
  }
  lower_quadrant
}
