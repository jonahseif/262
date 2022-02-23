
#' Damages to Sea Level Rise
#'
#' This function determines the damages to the coast due to sea level rise 
#' @param r is the sea level rise (m)
#' @param c is the length of the coast line (m)
#' @param d for value of water rise inland (m) 
#' @param d_conv is the damage conversion coefficient ($/m^3) default = 1,000,000 
#' @return damages ($)

searise = function(r, c, d, d_conv = 100000) {
  d = 2*r
  r = ifelse(r < 0, return("searise cannot be negative"), r)
  damages = r * c * d * d_conv
  return(damages)
}
