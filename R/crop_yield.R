#' Annual Crop Yield
#' 
#' This function determines the annual crop yield given the fertilizer application and precipitation
#' @param TP precipitaion (cm)
#' @param fertilizer
#' @param q is constant (1.8)
#' @param s is constant (0.5)
#' @param c is constant (0.3)
#' @return yield
#'
# function definition
crop_yield = function(fertilizer, TP, q = 1.8, s = 0.5, c = 0.3) {
fertilizer = ifelse(fertilizer < 0, return("fertilizer cannot be negative"), fertilizer)
result = q * fertilizer^2 - s * fertilizer + c*TP
total = sum(result)
return(total)
}