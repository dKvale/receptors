#' Fixed width strings
#'
#' Adjust the length of a character string by by adding spaces to the end if too short or cutting from the end if too long.
#' @param string Text to adjust. Can be passed a list.
#' @param nchars Number of characters or width to make each string.
#' @keywords fixed width fw cut string
#' @export
#' @examples
#' fw(c("2short", "cutThisString"), nchars = 10)
# 
# 

# Extend or cut a string to a set length
fw <- function(string = "cutThisString", 
               nchars = 10, ...) {

    new_string <- sprintf(paste0("%-", nchars, "s"), string)
    
    return(substr(new_string, 1, nchars))
}
