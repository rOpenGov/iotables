#' Create an output-independent, well formatted kable table
#' 
#' @param data_table data.frame, tibble, named matrix or a knitr_kable object.
#' @param caption A table caption, defaults to empty \code{NA}.
#' @param digits Number of digits to display in the case of numeric variables.
#' @param col.names The col.names parameter of the kable table, if \code{NULL},
#' filled with the names of the \code{data_table}.
#' @param width_unit Defaults to \code{"cm"}.
#' @param col_width Defaults to \code{NULL}. In this case all col_align parameters
#' will be \code{"c"} for centered.
#' @param col_align Defaults to \code{NULL}. In this case \code{'l'} for the
#' first column and \code{'r'} for the rest of the columns, 
#' i.e.\code{"l", "c", ..., "c"} 
#' @param border_right_cols, Defaults to \code{NULL}. 
#' In this case \code{TRUE} for the first column and \code{FALSE}
#' for the rest of the columns, i.e.\code{T, F, ..., F} 
#' @param bold_cols Defaults to \code{NULL}. In this case none of the 
#' the columns are bold, i.e. identical to \code{F, F, ..., F}.
#' @param bootstrap_options Defaults to \code{c("striped", "hover", "condensed")} and
#' only used for \code{output_format = "html"}.
#' @param latex_options Defaults to \code{NULL)}.
#' @param output_format Defaults to \code{'html'}. Alternatives are \code{'latex'}. 
#'  \code{'image'}, recommended for Word files, is removed because it depends
#'  on magick which is not available on all R platforms.
#' @param keep_pdf Defaults to  \code{FALSE} and only used if \code{output_format = 'image'}.
#' @param latex_header_includes Currently defaults to \code{c(
#' "\\usepackage[magyar]{babel}",
#' "\\usepackage[utf8]{inputenc}")}
#' It can be any valid latex option setting, but if packages are used, the 
#' packages must be installed on your Latex engine.
#' @importFrom knitr kable is_latex_output is_html_output
#' @importFrom kableExtra kable_styling column_spec
#' @keywords internal
#' @examples 
#' foo = data.frame ( 
#'   observation = c("indicator1", "indicator2", "indicator3"), 
#'   indicator_1 = c(100,105,95), 
#'   indicator_2 = c(102,104,76)
#'   )
#'   

create_knitr_table <- function ( 
                    data_table,
                    digits = NULL,
                    caption = NA,
                    col.names = NULL, 
                    col_width= NULL, 
                    width_unit = "cm",
                    col_align =NULL,
                    border_right_cols = NULL, 
                    bold_cols = NULL, 
                    bootstrap_options = c("striped", "hover", "condensed"), 
                    latex_options = NULL,
                    output_format = NULL, 
                    keep_pdf = FALSE, 
                    latex_header_includes = c(
                      "\\usepackage[magyar]{babel}",
                      "\\usepackage[utf8]{inputenc}")
                    ) {

  if ( is.null(ncol(data_table))) {
    stop ( "Empty data table inputed.")
  }
  
  if( is_latex_output() ) {
    output_format <- "latex"
  } else if ( is_html_output()) {
    output_format <- "html"
  }
  
  
  if ( is.null(output_format)) {
    output_format <- 'image'
  } 
  
  if (! output_format %in% c("latex", 'html') ) {
    output_format <- 'image'
  }
      
  
  
  if ( ! "knitr_kable" %in% class(data_table) ) { 
    #If columns are not named, col heads are names of data.frame
    if ( is.null(col.names)) {
      col.names = names(data_table)
    }
    
    #Create default column alignment settings if necessary
    if ( is.null(col_align)) {
      col_align = c('l', rep('c', (ncol(data_table)-1)))
    }
    
    #Create default column alignment settings if necessary
    if ( is.null(border_right_cols) ) {
      border_right_cols = c(TRUE, rep(FALSE, ncol(data_table)-1))
    }
    
    #Create default column alignment settings if necessary
    if ( is.null(bold_cols) ) {
      bold_cols = rep(FALSE, ncol(data_table))
    }
    
    if ( is.null(col_width)){
      col_width = c(2, 18/(ncol(data_table)-1))
    }
    
    #Create default digit settings----
    if ( is.null(digits)) {
      digits <- getOption("digits")
    } else if ( digits < 0 ) { digits <- getOption("digits") }
    
    col_width <- paste0(col_width, width_unit)   
    
    if ( output_format == "image" ) { 
      table_format <- 'latex' 
    } else if ( output_format %in% c("latex", "html") ) {
      table_format <- output_format 
    } else {
      table_format <- "html"
    }
    
    #Create the basic table
    knitr_table <- knitr::kable(
      data_table,
      digits = digits,
      format = table_format,  
      format.args = (list(big.mark = " ", decimal.mark = ",")),
      caption = caption,
      booktabs = TRUE, 
      col.names =  col.names, 
      align= col_align
    ) 
    
    }

  ## bootstrap_options for html tables and latex_options for latex
  if ( table_format == "html") { 
    if ( is.null(bootstrap_options)) {
      if (table_format == "html")   bootstrap_options <- c("striped", "hover", "condensed")
    }
    knitr_table <-  kableExtra::kable_styling(
           kable_input = knitr_table, 
           bootstrap_options = bootstrap_options) 
    }

  if ( table_format %in% c("latex", "image")  ) { 
    if ( is.null(latex_options) ) {
      if ( table_format == "latex") latex_options <- c("scale_down", "striped") #, "HOLD_position"
      if ( table_format == "image") latex_options <- c("scale_down", "striped") #"HOLD_position"
    }
    
    knitr_table <-  kableExtra::kable_styling(kable_input = knitr_table, 
                                              latex_options = latex_options) 
  }
  
  ###Add formatting elements from vectors col by col
  i <- 1
  while (i <= ncol(data_table)) {
    knitr_table <-  kableExtra::column_spec(kable_input = knitr_table,
                                            column = i, 
                                            width        = col_width[i], 
                                            bold         = bold_cols[i], 
                                            border_right = border_right_cols[i]
    )
    i = i + 1
  } #end of while loop


  knitr_table
  
} #end of function



  