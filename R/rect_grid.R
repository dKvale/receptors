#' Rectangle grid
#'
#' Generate a rectangular receptor grid
#' @param center_x Grid's center X coordinate.
#' @param center_y Grid's center Y coordinate.
#' @param length Length of receptor grid in the x direction.
#' @param width Width of receptor grid in the y direction.
#' @param spacing_x Distance between receptors in the x direction.
#' @param spacing_y Distance between receptors in the y direction.
#' @param show_plot Plot receptor grid.
#' @keywords receptors grid rectangle
#' @export
#' @examples
#' rect_grid(center_x = 0,
#'           center_y = 0,
#'           length   = 100,
#'           width    = 200,
#'           spacing_x  = 7,
#'           spacing_y  = 7)
#


rect_grid <- function(center_x       = 50,
                      center_y       = 50,
                      length         = 100,
                      width          = 200,
                      spacing_x      = 7,
                      spacing_y      = 7,
                      show_plot  = TRUE)
{

  receptors <- expand.grid(x = seq((center_x - floor((length/2) / spacing_x) * spacing_x),
                                   (center_x + floor((length/2) / spacing_x) * spacing_x),
                                   spacing_x),
                           y = seq((center_y - floor((width/2) / spacing_y) * spacing_y),
                                   (center_y + floor((width/2) / spacing_y) * spacing_y),
                                   spacing_y))

  # Plot receptors
  if(show_plot) plot(receptors$x, receptors$y, pch = "1")

  receptors$id <- 1:nrow(receptors)

  return(receptors[ , c(3,1,2)])

}


