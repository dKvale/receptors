#' Rectangle grid
#'
#' Generate a rectangular receptor grid
#' @param center_x Grid's center X coordinate.
#' @param center_y Grid's center Y coordinate.
#' @param width_x Width of receptor grid on the X axis.
#' @param length_y Length of receptor grid on the Y axis.
#' @param spacing_x Distance between receptors on the X axis.
#' @param spacing_y Distance between receptors on the Y axis.
#' @param inner_x Width of inner buffer to leave empty.
#' @param inner_y Length of inner buffer to leave empty.
#' @param show_plot Plot receptor grid.
#' @keywords receptors grid rectangle
#' @export
#' @examples
#' rect_grid(center_x   = 0,
#'           center_y   = 0,
#'           length_x   = 100,
#'           width_y    = 200,
#'           spacing_x  = 7,
#'           spacing_y  = 7)
#

rect_grid <- function(center_x       = 50,
                      center_y       = 50,
                      width_x        = 100,
                      length_y       = 200,
                      spacing_x      = 7,
                      spacing_y      = 7,
                      inner_x        = NULL,
                      inner_y        = NULL,
                      show_plot      = TRUE) {

  receptors <- expand.grid(x = seq((center_x - floor((width_x/2) / spacing_x) * spacing_x),
                                   (center_x + floor((width_x/2) / spacing_x) * spacing_x),
                                   spacing_x),
                           y = seq((center_y - floor((length_y/2) / spacing_y) * spacing_y),
                                   (center_y + floor((length_y/2) / spacing_y) * spacing_y),
                                   spacing_y))

  # Remove receptors within the inner boundary
  if(!is.null(inner_x) & !is.null(inner_y)) {
    
    if(inner_x < 0 | inner_y < 0) stop("Inner dimensions must be greater than zero.") 
    
    receptors <- subset(receptors, !(abs(x - center_x) < (inner_x / 2) & abs(y - center_y) < (inner_y / 2)))
  }
  
  # Plot receptors
  if(show_plot) graphics::plot(receptors$x, receptors$y, pch = "1")

  receptors$id <- 1:nrow(receptors)

  return(receptors[ , c(3,1,2)])

}


