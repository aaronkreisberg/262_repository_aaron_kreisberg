#' Function to calculate NDVI
#' @param NIR near infra-red satellite band (µm)
#' @param R Red satellite band (µm)
#' @return NDVI (µm)
# Equation
calc_ndvi <- function(NIR, R){
  ndvi <- (NIR - R)/(NIR + R)
  return(ndvi)
}
