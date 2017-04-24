#' Forage Ratio (Strauss 1979).
#'
#' Returns forage ratio from vectors of consumed and available food items.
#' @param available A vector of food items available to the organism in the environment
#' @param consumed A vector of food items consumed by the organism
#' @keywords forage ratio
#'
#' @examples
#' availableprey <- c(10,10,10,10,10)
#' consumedprey <- c(9,0,0,1,5)
#' FR(available = availableprey, consumed = consumedprey)
#'
#' @export
FR <- function(available, consumed) {
  r <- consumed/sum(consumed)
  p <- available/sum(available)

  return (r/p)
}

#' Ivlev's electivity (Ivlev 1961).
#' Returns Ivlev's electivity index from vectors of consumed and available food items.
#' @param available A vector of food items available to the organism in the environment
#' @param consumed A vector of food items consumed by the organism
#' @param jacob Converts to Jacob's electivity index? Defaults to FALSE.
#' @keywords ivlev selectivity
#'
#' @examples
#' availableprey <- c(10,10,10,10,10)
#' consumedprey <- c(9,0,0,1,5)
#' ivlev(available = availableprey, consumed = consumedprey, jacob = FALSE)
#' ivlev(available = availableprey, consumed = consumedprey, jacob = TRUE)
#'
#' @export
ivlev <- function(available, consumed, jacob = FALSE) {
  r <- consumed/sum(consumed)
  p <- available/sum(available)

  if(jacob == TRUE) {
    return (r-p)/(r+p-(2*r*p))
  } else {
    return ((r-p)/(r+p))
  }
}
