#' Read an AERMOD .rou receptor file into a data frame
#'
#' Read an AERMOD .rou receptor file into a data frame of receptor coordinates.
#' @param file Either a path to a file, a connection, or a text string.
#' @keywords receptors aermod read rou
#' @export
#' @examples
#' \dontrun{
#' read_rou(file = "circle_receptors.rou")
#' }
# 

read_rou <- function(file) {
  
  data <- readLines(file)
  
  # Filter to lines with text
  data <- data[grep("[A-z]", data)]
  
  start_re <- grep("RE", data)[[1]]
  
  if(length(data[-1]) < 1) return("File is missing coordinate data. At least 1 row of coordinates is required.")
  
  data <- trimws(data)
  
  elev_units   <- strsplit(data[start_re], " ")[[1]][3]
  
  # Create data frame
  data <- read.fwf(file   = textConnection(data), 
                   header = FALSE, 
                   widths = c(13, 12 ,15, 11),
                   skip   = start_re,
                   stringsAsFactors = FALSE)
  
  # Create column names                                                               
  names(data) <- c("grid_type", "x", "y", "elevation")
  
  # Trim whitespace
  data$grid_type <- trimws(data$grid_type)
  
  # Add elevation units to data frame                                                              
  data$elev_units <- elev_units                                                             
                                                                 
  # Return receptor data frame
  cat("\nReceptor data frame: \n\n")
  invisible(data)
  
  return(data)
  
}

