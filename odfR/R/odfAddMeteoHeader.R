# Add the meteorological information (METEO_HEADER) to the specified oce odf object.

# Author: Jeff Jackson
# Created: 18-FEB-2021
# Last Updated: 19-FEB-2021
#

# source("define_ODF_header.R")

#' odfAddMeteoHeader
#'
#' @param odf an oce odf object
#'
#' @return an oce odf object
#' @export
#'
#' @examples
#'
odfAddMeteoHeader <- function(odf){
  
  # Get the ODF structure information
  dODF <- define_ODF_header()
  
  # Get the METEO_HEADER fields
  ff <- dODF$METEO_HEADER
  
  # Create METEO_HEADER
  MH <- list()
  MH$METEO_HEADER <- list()
  for (i in 2:length(ff[,1])) {
    if (ff[i,2] == "char") {
      eval(parse(text = paste0("MH$METEO_HEADER['", ff[i,1], "'] = \"\"")))
    } else if (ff[i,2] == "numeric") {
      eval(parse(text = paste0("MH$METEO_HEADER['", ff[i,1], "'] = -99")))
    }
  }
  
  # Add the meteo header to the odf object
  odf <- append(odf, MH, 3)
  
  # Return the updated odf object
  return(odf)
  
}
