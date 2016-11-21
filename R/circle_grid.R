#' Circle grid
#'
#' Generate a circular receptor grid
#' @param center_x Grid's center X coordinate.
#' @param center_y Grid's center Y coordinate.
#' @param radius Radius of receptor grid.
#' @param spacing_x Distance between receptors in the x direction.
#' @param spacing_y Distance between receptors in the y direction.
#' @param show_plot Plot receptor grid.
#' @keywords receptors grid circle
#' @export
#' @examples
#' circle_grid(center_x   = 0,
#'             center_y   = 0,
#'             radius     = 100,
#'             spacing_x  = 7,
#'             spacing_y  = 7)
#

circle_grid <- function(center_x    = 50,
                        center_y    = 50,
                        radius      = 100,
                        spacing_x   = 7,
                        spacing_y   = 7,
                        show_plot   = TRUE)
{

  receptors <- expand.grid(x = seq((center_x - floor((radius) / spacing_x) * spacing_x),
                                   (center_x + floor((radius) / spacing_x) * spacing_x),
                                   spacing_x),
                           y = seq((center_y - floor((radius) / spacing_y) * spacing_y),
                                   (center_y + floor((radius) / spacing_y) * spacing_y),
                                    spacing_y))


  receptors <- subset(receptors, sqrt((receptors$x - center_x)**2 + (receptors$y - center_y)**2) <= radius)

  # Plot receptors
  if(show_plot) plot(receptors$x, receptors$y, pch = "1")

  receptors$id <- 1:nrow(receptors)

  return(receptors[ , c(3,1,2)])

}


