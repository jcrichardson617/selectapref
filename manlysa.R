#' Manly's alpha feeding preference (Chesson 1968).
#' Returns Manly'as alpha index from vectors of initial and final food item counts.
#' @param initial A vector of initial food items counts available to the organism in the environment
#' @param consumed A vector of food items consumed by the organism
#' @param stand Converts values with highest standardized to "1"? Defaults to FALSE.
#' @param perc Converts values to percentages? Defaults to FALSE.
#' @param deplete For use in experiments where food sources deplete? Defaults to TRUE.
#' @keywords Manly's alpha preference selectivity
#'
#' @examples
#' initial_prey_count <- c(10,10,10,10,10,10)
#' number_prey_consumed <- c(9,8,1,3,5,9)
#' manlysalpha(initial = initial_prey_count, consumed = number_prey_consumed,
#' stand = TRUE, perc = FALSE, deplete = TRUE)
#'
#' manlysalpha(initial = initial_prey_count, consumed = number_prey_consumed,
#' stand = TRUE, perc = TRUE, deplete = TRUE)
#'
#' @export
manlysalpha <- function (initial, consumed, stand = FALSE, perc = FALSE, deplete = TRUE) {
  maconst <- ((consumed/initial)/(sum(consumed/initial)))
  madepl <- ((log((initial-consumed)/initial))/(sum(log((initial-consumed)/initial))))

  if(deplete == TRUE) {
    if(perc == TRUE) {
      if(stand == TRUE) {
        return((100*(1/max(madepl))*madepl))
      }
      {
      return (100*madepl)
      }
    }
    if(stand == TRUE) {
      return((1/max(madepl))*madepl)
    }
    return (madepl <- ((log((initial-consumed)/initial))/(sum(log((initial-consumed)/initial)))))
  } else {
    if(perc == TRUE) {
      if(stand == TRUE) {
        return(100*((1/max(maconst))*maconst))
      }
      {
      return (100*maconst)
      }
    }
    if(stand == TRUE) {
      return((1/max(maconst))*maconst)
    }
    return (maconst)
  }
}
