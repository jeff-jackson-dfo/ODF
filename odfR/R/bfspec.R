#' @title Create an ODF file specification string.
#' 
#' @description 
#' 'bfspec' returns a file specification string based on the metadata of the input odf oce object.
#'
#' @author Jeff Jackson
#'
#' @version 1.0
#'
#' @param C an odf oce object.
#'
#' @return bstr The file specification string.
#'
#' @seealso odfR::bfname(), oce
#'
#' @export
#'
#' @examples
#' bfstr <- bfspec(ODF)
#'
#' Creation Date: 02-SEP-2015
#' Last Updated: 20-FEB-2021
#'
bfspec <- function(C) {
  ch <- C@metadata$odfHeader$CRUISE_HEADER
  eh <- C@metadata$odfHeader$EVENT_HEADER
  bstr <- paste(eh$DATA_TYPE, '_', ch$CRUISE_NUMBER, '_', eh$EVENT_NUMBER, '_', eh$EVENT_QUALIFIER1, '_', eh$EVENT_QUALIFIER2, sep = "")
  return(bstr)
}
