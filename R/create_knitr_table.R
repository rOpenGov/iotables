#' Create an output-independent, well formatted kable table
#' 
#' @param data_table data.frame, tibble, named matrix or a knitr_kable object.
#' @param caption A table caption, defaults to empty \code{NA}.
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
#' @param bootstrap_options Defaults to \code{c("striped", "hover", "condensed")} and
#' only used for \code{output_format = "html"}.
#' @param latex_options Defaults to \code{NULL)}.
#' @param output_format Defaults to \code{'html'}. Alternatives are \code{'latex'} and
#' \code{'image'}, recommended for Word files.
#' @param keep_pdf Defaults to  \code{FALSE} and only used if \code{output_format = 'image'}.
#' @param latex_header_includes Currently defaults to \code{c(
#' "\\usepackage[magyar]{babel}",
#' "\\usepackage[utf8]{inputenc}")}
#' It can be any valid latex option setting, but if packages are used, the 
#' packages must be installed on your Latex engine.
#' @importFrom knitr kable
#' @importFrom kableExtra kable_as_image kable_styling column_spec
#' @examples 
#' foo = data.frame ( 
#'   observation = c("indicator1", "indicator2", "indicator3"), 
#'   indicator_1 = c(100,105,95), 
#'   indicator_2 = c(102,104,76)
#'   )

create_knitr_table <- function ( 
                    data_table, 
                    caption = NA,
                    col.names = NULL, 
                    col_width= c(2,6,6), 
                    width_unit = "cm",
                    col_align =c("l", "c", "c"),
                    border_right_cols = c(T, F, F), 
                    bold_cols = c(F, F, F), 
                    bootstrap_options = c("striped", "hover", "condensed"), 
                    latex_options = NULL,
                    output_format = "html", 
                    keep_pdf = FALSE, 
                    latex_header_includes = c(
                      "\\usepackage[magyar]{babel}",
                      "\\usepackage[utf8]{inputenc}")
                    ) {

  if ( ! "knitr_kable" %in% class(data_table) ) { 
    #If columns are not named, col heads are names of data.frame
    if ( is.null(col.names)) {
      col.names = names(data_table)
    }
    
    #Create default column alignment settings if necessary
    if ( is.null(col_align)) {
      col_align = c('l', rep('c', ncol(data_table)-1))
    }
    
    #Create default column alignment settings if necessary
    if ( is.null(border_right_cols) ) {
      col_align = c(TRUE, rep(FALSE, ncol(data_table)-1))
    }
    
    col_width <- paste0(col_width, width_unit)   
    
    if ( output_format == "image" ) { 
      format <- 'latex' 
    } else if ( output_format %in% c("latex", "html") ) {
      format <- output_format 
    } else {
      format <- "html"
    }
    
    #Create the basic table
    knitr_table <- knitr::kable(
      data_table,
      format = format,  
      format.args = (list(big.mark = " ", decimal.mark = ",")),
      caption = caption,
      booktabs = TRUE, 
      col.names =  col.names, 
      align=col_align
    ) 
    
    }
  
  
  ## bootstrap_options for html tables and latex_options for latex
  if ( format == "html") { 
    if ( is.null(bootstrap_options)) {
      if (format == "html")   bootstrap_options <- c("striped", "hover", "condensed")
    }
    knitr_table <-  kableExtra::kable_styling(
           kable_input = knitr_table, 
           bootstrap_options = bootstrap_options) 
    }

  if ( format %in% c("latex", "image")  ) { 
    if ( is.null(latex_options) ) {
      if ( format == "latex") latex_options <- c("scale_down", "striped", "HOLD_position")
      if ( format == "image") latex_options <- c("striped", "HOLD_position")
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
                                            bold         = bold_columns[i], 
                                            border_right = border_right_cols[i]
    )
    i = i + 1
  } #end of while loop
  
  if ( output_format == 'image') {
    knitr_table <- kableExtra::kable_as_image(
         kable_input = knitr_table,
         keep_pdf = keep_pdf,
         latex_header_includes = latex_header_includes)
  }
  

  knitr_table
  
} #end of function



  