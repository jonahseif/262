#' Automobile Fuel Efficiency
#' 
#' This function determines automobile fuel efficiency by calculating the power required to keep a car moving at a given speed
#' @param V vehicle speed (m/s)
#' @param m vehicle mass (kg)
#' @param A vehicle surface area (m2)
#' @param g acceleration due to gravity (m/s2) default=9.8
#' @param p_air density of air (kg/m3) default=1.2
#' @param crolling rolling coefficient default=0.015
#' @param c_drag aerodynamic resistive coefficient default=0.3
#' @return power (W)
#' 
# function definition 
vehicle_power = function(speed, mass, area, g=9.8, p_air=1.2, crolling=0.015, c_drag=0.3) {
result = crolling * mass * g * speed + 1/2 * area * p_air * c_drag * speed^3
return(result)
}