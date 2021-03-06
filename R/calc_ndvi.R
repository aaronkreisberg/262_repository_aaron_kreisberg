#' Function to calculate NDVI
#' @param NIR near infra-red satellite band (�m)
#' @param R Red satellite band (�m)
#' @return NDVI (�m)
# Equation
calc_ndvi <- function(NIR, R){
  ndvi <- (NIR - R)/(NIR + R)
  return(ndvi)
}
