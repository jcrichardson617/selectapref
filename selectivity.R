#' Forage Ratio (Strauss 1979).
#'
#' Returns forage ratio from vectors of consumed and available food items.
#' @param available A vector of food items available to the organism in the environment
#' @param consumed A vector of food items consumed by the organism
#' @keywords forage ratio
#' 
#' 

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

ivlev <- function(available, consumed, jacob = FALSE) {
  r <- consumed/sum(consumed)
  p <- available/sum(available)
  
  if(jacob == TRUE) {
    return (r-p)/(r+p-(2*r*p))
  } else {
    return ((r-p)/(r+p))
  }
}