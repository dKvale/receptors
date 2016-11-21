#' Write .rou receptor file 
#'
#' Write a .rou receptor file for AERMOD from a dataframe of receptor coordinates.
#' @param data Dataframe of receptor coordinates and elevations. Requires columns: `x`, `y`, and `elevation`.
#' @param path Path to write to. If NULL, returns result as text.
#' @param grid_type Type of receptor grid for AERMOD. Default is "disccart" (discrete cartesian).
#' @param elev_units Units for elevation. Default is "meters".
#' @keywords receptors aermod write save rou
#' @export
#' @examples
#' receptors <- circle_grid(center_x = 50, center_y= 50, radius = 100)
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
                      elev_units  = "meters", 
                      ...) {
  
  if(nrow(data) < 1) return("Data frame is empty. At least 1 receptor is required.")
  
  rou <- c(paste0("** \n", "RE ELEVUNIT METERS\n"),
                 paste0("  ", toupper(grid_type), "     ", 
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
  