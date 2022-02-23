
#' Automobile Power Generation
#' 
#' This function computes automobile fuel efficiency by a given vehicle speed, mass, and area
#' @param speed vehicle speed (m/s)
#' @param mass vehicle mass (kg)
#' @param area Surface area of car (m^2)
#' @param g acceleration due to gravity (m/s^2) default=9.8
#' @param rho_air density of air (kg/m^2) default=1.2
#' @param crolling rolling coefficient default=0.015
#' @param cdrag aerodynamic resistive coefficeint default=0.3
#' @reutrn power (Watts)
#' 
#' function definition
 auto_power_gen_orig = function(crolling=0.015, mass, g=9.8, speed, area, rho_air=1.2, cdrag=0.3) {result = crolling * mass * g * speed + 0.5 * area * rho_air * cdrag * speed^3
 return(result)
 }
 