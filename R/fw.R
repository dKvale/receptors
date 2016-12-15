#' Fixed width strings
#'
#' Adjust the length of a character string by by adding spaces to the end if too short, 
#' or by cutting from the end if too long.
#' @param string Text to adjust. Use a list or vector for multipe text strings.
#' @param n Number of characters for \code{string}. If negative, \code{string} will be shortened by \code{n} characters.
#' @keywords fixed width fw cut string
#' @export
#' @examples
#' fw(c("2short", "cutThisString"), n = 10)
# 
# 

fw <- function(string = "12345678910", 
               n      = 10) {
   
    if(is.null(n) || is.na(n)) n <- nchar(string) 
    
    if(n < 0) n <- nchar(string) + n
    
    if(length(n) != length(string)) n[1:length(string)] <- n
    
    new_string  <- sprintf(paste0("%-", n, "s"), string)
    
    new_string  <- substring(new_string, 1, n)
    
    return(new_string)
}
