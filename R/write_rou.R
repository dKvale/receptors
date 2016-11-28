#' Write .rou receptor file 
#'
#' Write a .rou receptor file for AERMOD from a data frame of receptor coordinates.
#' @param data Dataframe of receptor coordinates and elevations. Requires columns: `x`, `y`, and `elevation`.
#' @param path Path to write to. If NULL, returns result as text.
#' @param grid_type Type of receptor grid for AERMOD. Default is "disccart" (discrete cartesian).
#' @param elev_units Units for elevation. Default is "meters".
#' @keywords receptors aermod write save rou
#' @export
#' @examples
#' receptors <- data.frame(id = 0:5, x = 50:55, y= 50:55)
#' 
#' receptors$elevation <- 250
#' 
#' write_rou(data = receptors,
#'           path = "circle_receptors.rou")
# 
# 

write_rou <- function(data, 
                      path        = NULL,
                      grid_type   = "disccart",
                      elev_units  = "meters") {
  
  if(nrow(data) < 1) return("Data frame is empty. At least 1 receptor is required.")
  
  rou <- c(paste0("** \n", "RE ELEVUNIT METERS\n"),
                 paste0("   ", 
                        fw(toupper(grid_type), 13), 
                        fw(data$x, 12), 
                        fw(data$y, 15), 
                        fw(data$elevation, 11), collapse="\n")) 
  
  
  # Write AERMOD receptor file
  cat("\nGenerated receptor file: \n\n")
  invisible(writeLines(rou))
  
  if(is.null(path) || nchar(path) < 1) {
    return(rou)
  } else  writeLines(rou, path)
  
}
  
