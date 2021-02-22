#' as.gf3
#'
#' Translate parameter name to GF3 code
#'
#' @param VARNAME the common name for known variables or the GF3 param code if known
#'
#' @return list with GF3 code, def, width, prec and units
#' @export
#'
#' @examples
#' 
as.gf3 <- function(VARNAME) {

  load("../data/gf3defs.RData")
  
  if (!(VARNAME %in% gf3defs$GF3_CODE)) {
    
    if (VARNAME == 'u'){
      codevar <- 'EWCT'
    }
    if (VARNAME == 'v'){
      codevar <- 'NSCT'
    }
    if (VARNAME == 'w'){
      codevar <- 'VCSP'
    }
    if (VARNAME == 'errv'){
      codevar <- 'ERRV'
    }
    if (VARNAME == 'pgd'){
      codevar <- 'PGDP'
    }
    if (VARNAME == 'agc'){
      codevar <- 'BEAM'
    }
    if (VARNAME == 'sytm') {
      codevar <- 'SYTM'
    }
    if (VARNAME == 'conductivity') {
      codevar <- 'CNDC'
    }
    if (VARNAME == 'conductivity ratio') {
      codevar <- 'CRAT'
    }
    if (VARNAME == 'salinity') {
      codevar <- 'PSAL'
    }
    if (VARNAME == 'temperature') {
      codevar <- 'TEMP'
    }
    if (VARNAME == 'sigmaTheta') {
      codevar <- 'SIGP'
    }
    if (VARNAME == 'theta') {
      codevar <- 'POTM'
    }
    if (VARNAME == 'density') {
      codevar <- 'DENS'
    }
    if (VARNAME == 'oxygen') {
      codevar <- 'DOXY'
    }
    if (VARNAME == 'oxygenVoltage') {
      codevar <- 'OXYV'
    }
    if (VARNAME == 'pressure') {
      codevar <- 'PRES'
    }
    if (VARNAME == 'scan') {
      codevar <- 'CNTR'
    }
    if (VARNAME == 'scan count') {
      codevar <- 'SNCN'
    }
    if (VARNAME == 'depth') {
      codevar <- 'DEPH'
    }
    if (VARNAME == 'dissolvedosaturation') {
      codevar <- 'OSAT'
    }
    if (VARNAME == 'flag') {
      codevar <- 'FFFF'
    }
  } else {
    codevar <- VARNAME
  }
  
  #add more oce to gf3 code translations
  #eg for ctd, cm, tr, etc
  loc <- gf3defs$GF3_CODE == codevar
  VARNAME <- list(
    code = gf3defs$GF3_CODE[loc],
    def = gf3defs$GF3_DEFINITION[loc],
    units = gf3defs$UNITS[loc],
    width = gf3defs$WIDTH[loc],
    prec = gf3defs$PRECISION[loc]
  )
  return(VARNAME)
}
