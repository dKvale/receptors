#' Circle grid
#'
#' Generate a circular receptor grid
#' @param center_x Grid's center X coordinate.
#' @param center_y Grid's center Y coordinate.
#' @param radius Radius of receptor grid.
#' @param spacing_x Distance between receptors on the X axis.
#' @param spacing_y Distance between receptors on the Y axis.
#' @param inner_radius Radius of inner buffer left empty of receptors. 
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

circle_grid <- function(center_x     = 50,
                        center_y     = 50,
                        radius       = 100,
                        spacing_x    = 7,
                        spacing_y    = 7,
                        inner_radius = NULL,
                        show_plot    = TRUE) {

  receptors <- expand.grid(x = seq((center_x - floor((radius) / spacing_x) * spacing_x),
                                   (center_x + floor((radius) / spacing_x) * spacing_x),
                                   spacing_x),
                           y = seq((center_y - floor((radius) / spacing_y) * spacing_y),
                                   (center_y + floor((radius) / spacing_y) * spacing_y),
                                    spacing_y))


  # Remove receptors further away than designated radius
  receptors <- subset(receptors, sqrt((receptors$x - center_x)**2 + (receptors$y - center_y)**2) <= radius)

  # Remove receptors within the inner radius
  if(!is.null(inner_radius)) {
     if(inner_radius < 0) stop("inner_radius must be greater than zero.") 
     receptors <- subset(receptors, sqrt((receptors$x - center_x)**2 + (receptors$y - center_y)**2) > inner_radius)
  }
  
  # Plot receptors
  if(show_plot) graphics::plot(receptors$x, receptors$y, pch = "1")

  receptors$id <- 1:nrow(receptors)

  return(receptors[ , c(3,1,2)])

}


