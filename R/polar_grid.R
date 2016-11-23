#' Polar grid
#'
#' Generate a polar receptor grid
#' @param center_x Grid's center X coordinate.
#' @param center_y Grid's center Y coordinate.
#' @param radii Distances of receptor circles from center.
#' @param n_radials Number of radials / spokes of receptors.
#' @param show_plot Plot receptor grid.
#' @keywords receptors grid polar
#' @export
#' @examples
#' polar_grid(center_x = 0,
#'            center_y = 0,
#'            length = 1,
#'            width = 2)
#


polar_grid <- function(center_x   = 100,
                       center_y   = 100,
                       radii      = c(10,15,20),
                       n_radials  = 36,
                       show_plot  = TRUE, ...)
  {


  receptors <- data.frame(id  = numeric(),
                          x   = numeric(),
                          y   = numeric())

  for(radius in radii) {

    circumf <- 2 * pi * radius

    for(radial in 1:n_radials) {

      new_id <- nrow(receptors) + 1

      angle_radians <- 2 * pi * radial / n_radials

      new_x <- center_x + radius * cos(angle_radians)

      new_y <- center_y + radius * sin(angle_radians)

      receptors[new_id, ] <- list(new_id, signif(new_x, 8), signif(new_y, 9))
    }
  }

  # Plot receptors
  if(show_plot) plot(receptors$x, receptors$y, pch = "1")

  return(receptors)

}

