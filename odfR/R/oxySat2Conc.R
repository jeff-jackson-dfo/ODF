#' oxySat2Conc
#'
#' This is a R version of the RBR Ltd. MATLAB function deriveO2concentration.m
#' This function uses the Weiss (1970) formula.
#'
#' @author Jeff Jackson
#' @date 22-FEB-2021
#'
#' @param oxsat oxygen saturation (%)
#' @param temp temperature of the water (degrees C)  
#' @param sal salinity of the water (PSU)
#' @param unit unit for the data to be output
#' 
#' @return vector of dissolved oxygen values in the specified input unit
#'
#' @examples
#' # 1. Convert an oxygen saturation value to oxygen concentration
#' > oxySat2Conc(74.0359532690906, 4.43346635518515, 32.1274, 'ml/l')
#' [1] 5.423054
#' 
#' # 2. Convert an oxygen saturation value to oxygen in micromolars
#' > oxySat2Conc(74.0359532690906, 4.43346635518515, 32.1274, 'mumol/l')
#' [1] 242.1882
#' 
#' 
oxySat2Conc <- function(oxsat, temp, sal, unit) {

  a1 <- -173.42920
  a2 <- 249.63390
  a3 <- 143.34830
  a4 <- -21.84920
  b1 <- -0.0330960
  b2 <- 0.0142590
  b3 <- -0.00170
  
  temp <- (temp * 1.00024 + 273.15) / 100.0
  
  oxconmll <- oxsat * exp(a1 + (a2 / temp) + (a3 * log(temp, exp(1))) + (a4 * temp) + sal * (b1 + (b2 * temp) + (b3 * temp^2))) / 100.0
  
  if (unit == 'mumol/l') {
    # default, convert to mumol/l (i.e. µmol/l)
    oxcon <- 44.659 * oxconmll
  } else if (unit == 'ml/l') {
    # ml/l
    oxcon <- oxconmll
  } else if (unit == 'mg/l') {
    # convert to mg/l
    oxcon <- 1.4276 * oxconmll
  }
  
  return(oxcon)

}
