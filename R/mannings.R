
#' Manning's Equation
#'
#' This function determines flow rate in an an open channel 
#'
#' @param a = Flow Area, (ft2)
#' @param n = Manning's Roughness Coefficient
#' @param r = Hydraulic Radius, (ft)
#' @param s= Channel Slope, (ft/ft)
#' @return q = Flow Rate, (ft3/s)
#'
#' function definition

mannings = function(a= 3000, r, n, s) {
  if (s < 0) return("slope must be greater then zero")
    q = (1.49/n)*(a)*(r^(2/3))*(s^(1/2))
  return(q)
} 