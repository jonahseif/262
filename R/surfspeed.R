
#'Speed of a surfer
#'
#'This function determines the speed of a surfer as they hit the bottom of the wave just after take off
#'@param m mass of surfer in kg
#'@param v speed of wave (m/s)
#'@param h height of wave (m)
#'@param g speed of gravity (m/s) = 9.8
#'@param cd drag coefficient of seawater =  0.04- drag of a streamlined body
#'@param r density of seawater = 1.02
#'@param A area of surfboard (m^2)
#'@return s speed of surfer (m/s)
#'
# function definition
surfer_speed = function(m, v, h, A, g=9.8, cd= 0.04 , r=1.02){
   surfer_speed= ((m*v^2 + 2*m*g*h)/(m+r*A*cd))^(1/2)
}