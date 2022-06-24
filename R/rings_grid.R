#' Rings grid
#'
#' Generate rings of receptors at increasing distances from a center point.
#' @param center_x Grid's center X coordinate.
#' @param center_y Grid's center Y coordinate.
#' @param radii Distances of receptor rings from center.
#' @param spacing Distance between receptors. 
#'                A single value will apply to all rings. 
#'                Enter a vector of values to apply a unique spacing distance 
#'                for each ring.
#' @param show_plot Plot receptor grid.
#' @keywords receptors grid rings
#' @export
#' @examples
#' rings_grid(center_x = 0,
#'            center_y = 0,
#'            radii    = c(10,15,20,30,100),
#'            spacing  = 7)
#'
#

rings_grid <- function(center_x       = 0,
                       center_y       = 0,
                       radii          = c(10,15,20,30,100),
                       spacing        = 7,
                       show_plot      = TRUE) { 

  receptors <- data.frame(id  = numeric(),
                          x   = numeric(),
                          y   = numeric())
  
  # Expand spacing distance for each radii
  if(length(spacing) < length(radii)) spacing[length(spacing) : length(radii)] <- spacing[length(spacing)]

  for(i in 1:length(radii)) {

    circumf <- 2 * pi * radii[i]

    n_radials <- ceiling(circumf / spacing[i])

    for(radial in 1:n_radials) {

      new_id <- nrow(receptors) + 1

      angle_radians <- 2 * pi * radial / n_radials

      new_x <- center_x + radii[i] * cos(angle_radians)

      new_y <- center_y + radii[i] * sin(angle_radians)

      receptors[new_id, ] <- list(new_id, signif(round(new_x, 8), 8), signif(round(new_y, 9), 9))
    }

}

  # Plot receptors
  if(show_plot) graphics::plot(receptors$x, receptors$y, pch = "1"); points(center_x, center_y, col = "orange", pch=19)

  return(receptors)

}
