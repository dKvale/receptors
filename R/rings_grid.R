#' Rings grid
#'
#' Generate rings of receptors at increasing distances from a center point.
#' @param center_x Grid's center X coordinate.
#' @param center_y Grid's center Y coordinate.
#' @param radii Distances of receptor rings from center.
#' @param spacing Distance between receptors. A single value will apply to all rings. Enter a sequence of values to apply a unique distance for each ring.
#' @param show_plot Plot receptor grid.
#' @keywords receptors grid rings
#' @export
#' @examples
#' rings_grid(center_x = 0,
#'           center_y = 0,
#'           radii    = c(10,15,20,30,100),
#'           spacing  = 7)
#'
#

#spacing_fun    = "1 + spacing_factor * radius / 50"

rings_grid <- function(center_x      = 100,
                      center_y       = 100,
                      radii          = c(10,15,20,30,100),
                      spacing        = 7,
                      show_plot      = TRUE)
{

  receptors <- data.frame(id  = numeric(),
                          x   = numeric(),
                          y   = numeric())

  for(radius in radii) {

    #spacing <- eval(parse(text = spacing_fun))

    circumf <- 2 * pi * radius

    n_radials <- ceiling(circumf / spacing)

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


