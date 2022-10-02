#' degToRad function
#'
#' function to convert degrees to radians 
#'
#' @param deg numeric. degrees.
#' @return numeric. radians
#' @details convert degrees to radians 
#' @examples degToRad(45) == pi / 4
#' @export
degToRad <- function (deg) {deg * pi / 180}
