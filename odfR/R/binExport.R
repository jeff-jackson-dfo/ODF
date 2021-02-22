####ODF select bin export####

#' Select bin export
#'
#' @param obj an ODF structure object with multiple depth bins (produced from oce2odf)
#' @param bins the bin numbers you wish to export
#'
#' @return
#' @export
#'
#' @examples
#' ```
#' # oce2odf(adp, write = FALSE)
#' # bins <- list(12:34)
#' # binExport(obj = b, bins)
#' ````
binExport <- function(obj, bins) {
  for (l in bins) {
    write_odf(obj[[l]],
              output_file = paste0(obj[[l]]$ODF_HEADER$FILE_SPECIFICATION))
    
  }
}
