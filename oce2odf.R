####oce2odf####


#'oce2odfHeader
#'
#'creates ODF standard header from metadata within an oce object to be copied and replicated in ODF files
#'object can subsequently be used in function `oce2odf`
#'
#'returns empty odf object with header metadata filled out 
#'
#' @param obj oce object

#note before running please write
#obj[['event_comments']]
#obj[['description']]


oce2odfHeader <- function(obj){
  
  b <- gen_odfstruct()
  
  if (inherits(obj) == 'adp'){
    for ( d in 1:length(obj[['distance']])){
    b$ODF_HEADER$FILE_SPECIFICATION <- paste('MADCPS', '_', obj[['cruise_number']], '_', obj[['mooring_number']], '_', obj[['serial_number']] , '-', obj[['sensor_depth']] - obj[['distance']][[d]], '.ODF', sep = '')
    }
    b$CRUISE_HEADER$COUNTRY_INSTITUTE_CODE <- obj[['country_code']]
    b$CRUISE_HEADER$CRUISE_NUMBER <- obj[['cruise_number']]
    b$CRUISE_HEADER$ORGANIZATION <- obj[['organization']]
    b$CRUISE_HEADER$CHIEF_SCIENTIST <- obj[['chief_scientist']]
    b$CRUISE_HEADER$START_DATE <- obj[['time_coverage_start']]
    b$CRUISE_HEADER$END_DATE <- obj[['time_coverage_end']]
    b$CRUISE_HEADER$PLATFORM <- obj[['platform']]
    b$CRUISE_HEADER$CRUISE_NAME <- obj[['cruise_name']]
    b$CRUISE_HEADER$CRUISE_DESCRIPTION <- obj[['cruise_description']]
    
    
    b$EVENT_HEADER$DATA_TYPE <- obj[['data_type']]
    b$EVENT_HEADER$EVENT_NUMBER <- obj[['mooring_number']]
    b$EVENT_HEADER$EVENT_QUALIFIER1 <- obj[['']]
    b$EVENT_HEADER$EVENT_QUALIFIER2 <- obj[['']]
    b$EVENT_HEADER$CREATION_DATE <- Sys.Date()
    b$EVENT_HEADER$ORIG_CREATION_DATE <- obj[['date_created']]
    b$EVENT_HEADER$START_DATE_TIME <- obj[['time_coverage_start']]
    b$EVENT_HEADER$END_DATE_TIME <- obj[['time_coverage_end']]
    b$EVENT_HEADER$INITIAL_LATITUDE <- obj[['latitude']]
    b$EVENT_HEADER$INITIAL_LONGITUDE <- obj[['longitude']]
    b$EVENT_HEADER$END_LATITUDE <- obj[['latitude']]
    b$EVENT_HEADER$END_LONGITUDE <- obj[['longitude']]
    b$EVENT_HEADER$MIN_DEPTH <- min(obj[['depth']])     #CAUTION THESE ARE ONLY PLACEHOLDERS, EACH FILE SHOULD HAVE 
    b$EVENT_HEADER$MAX_DEPTH <- max(obj[['depth']])     #THESE VALUES REPLACED BY BIN DEPTH IN THE OCE2ODF FUNCTION
    b$EVENT_HEADER$SAMPLING_INTERVAL <- obj[['sampling_interval']]
    b$EVENT_HEADER$SOUNDING <- obj[['sounding']]
    b$EVENT_HEADER$DEPTH_OFF_BOTTOM  <- max(obj[['depth']]) - obj[['depthMean']]
    b$EVENT_HEADER$EVENT_COMMENTS <- obj[['event_comments']]
    
    
    b$INSTRUMENT_HEADER$INST_TYPE <- obj[['instrumentType']]
    b$INSTRUMENT_HEADER$MODEL <- obj[['model']]
    b$INSTRUMENT_HEADER$SERIAL_NUMBER <- obj[['serialNumber']]
    b$INSTRUMENT_HEADER$DESCRIPTION <- obj[['description']]
    
    return(b)
    
    
    
  }
}
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @param obj oce object for data to be copied to odf
#' @param write whether or not to write out all the odf files produced, default
#'   is TRUE, if false please use binExport to select the bins for which you
#'   would like to produce ODFs



oce2odf <- function(obj, write = TRUE){
  #identify type of oce object
  if (inherits(obj, what = 'adp') ){
    #file names
    names <- list()
    for( d in 1:length(obj[['distance']] )){
      #caUTION: oce uses snake case, ADCP process uses '_'
      names[[d]] <- paste('MADCPS', '_', obj[['cruise_number']], '_', obj[['mooring_number']], '_', obj[['serialNumber']] , '-',  round(obj[['distance']][[d]], digits = 0), '.ODF', sep = '')
      
    }
    #name variables to export to ODF
    params <- list('u', 'v', 'w', 'errv', 'pgd', 'agc')
    
    u <- obj[['v']][,,1]
    v <- obj[['v']][,,2]
    w <- obj[['v']][,,3]
    errv <- obj[['v']][,,4]
    pgd <- obj[['g', 'numeric']][,,4]
    agc <- apply(X = obj[['a', 'numeric']], MARGIN = 1:2, FUN = mean, na.rm = TRUE)   
   #handle time separately
     sytm <- obj[['time']]
    
    #work on add_parameter to make easier
    #split each data variable into single depth time series
    
      #add each section of data as parameter in loop of odf files by depths
      
      #creates data array which matches dimensions of variables, 
    b <- NULL
      for (d in 1: length(obj[['distance']])){
        b[[d]] <- gen_odfstruct()
        b[[d]]$DATA <- matrix(NA,  nrow = length(adp[['time']]),  ncol = length(params))
      }
    
     for (i in 1:length(params)){
        for( d in 1: length(obj[['distance']])){
            eval(parse(text = paste0("b[[d]]$DATA[,i] <- ", params[[i]], "[,d] ")))
          
          }
        }
      
    
    #handle time separately
    for (d in 1:length(obj[['distance']])){
      for(p in params)
      as.data.frame(b[[d]]$DATA)
      
      colnames(b[[d]]$DATA)<- list('EWCT_01', 'NSCT_01','VCSP_01', 'ERRV_01', 'PGDP_01', 'BEAM_01')
    }
    if (!is.null(obj[['time']])){
      SYTM_01 <- as.character(as.POSIXct(obj[['time']], origin = '1970-01-01 00:00:00'))
    for (d in 1:length(obj[['distance']])){
  b[[d]]$DATA <- cbind.data.frame(b[[d]]$DATA, SYTM_01)
    }
    }

    gf3 <- list()
    for( p in params){
      gf3[[p]] <- as.gf3(p)
    }
    for ( d in 1:length(obj[['distance']])){
      
      length(b[[d]]$PARAMETER_HEADER) <- length(b[[d]]$PARAMETER_HEADER) + length(params)
      for (i in 1:length(params)){
      b[[d]]$PARAMETER_HEADER[[i]] <- list(
        TYPE = 'SING',
        NAME = gf3[[params[[i]]]]$def,
        UNITS = gf3[[i]]$units,
        CODE = paste(gf3[[i]]$code , '01', sep = '_'),
        NULL_VALUE = '-1000000',
        PRINT_FIELD_WIDTH = as.character(gf3[[i]]$width),
        PRINT_DECIMAL_PLACES = as.character(gf3[[i]]$prec),
        ANGLE_OF_SECTION = '-1000000',
        MAGNETIC_VARIATION = '-1000000',
        DEPTH = round(obj[['depthMean']] - obj[['distance']][[d]], digits = 0),
        MINIMUM_VALUE = as.character(eval(parse(text = paste0("min(", params[[i]], ", na.rm = TRUE)")))),
        MAXIMUM_VALUE = as.character(eval(parse(text = paste0("max(", params[[i]], ", na.rm = TRUE)")))),
        NUMBER_VALID = as.character(eval(parse(text = paste0("length(na.omit(", params[[i]], "))")))),
        NUMBER_NULL = as.character(eval(parse(text = paste0("length(", params[[i]], ") - length(na.omit(" ,params[[i]], "))"))))
      )
      }
      if ( !is.null(obj[['time']])){
        s <- as.gf3('sytm')
      length(b[[d]]$PARAMETER_HEADER) <- length(b[[d]]$PARAMETER_HEADER) + 1
      i <- length(b[[d]]$PARAMETER_HEADER)
      b[[d]]$PARAMETER_HEADER[[i]] <- list(
        TYPE = 'SYTM',
        NAME = s$def,
        UNITS = s$units,
        CODE =  'SYTM_01',
        NULL_VALUE = '-99',
        PRINT_FIELD_WIDTH = s$width,
        PRINT_DECIMAL_PLACES = s$prec,
        ANGLE_OF_SECTION = '-99',
        MAGNETIC_VARIATION = '-99',
        DEPTH = '0',
        MINIMUM_VALUE = min(as.character(SYTM_01), na.rm = TRUE),
        MAXIMUM_VALUE = max(as.character(SYTM_01), na.rm = TRUE),
        NUMBER_VALID = length(na.omit(SYTM_01)),
        NUMBER_NULL = length(SYTM_01) - length(na.omit(SYTM_01))
      )
      }
    }
  
    
      
      #parameter header, polynomial cal header (optional), compass cal header
      #adds to history header with each action like oce processing log
      
      #add header block to each odf file (standard, same for each file)
      
      for( d in 1:length(obj[['distance']])){
        
        #ODF HEADER
        b[[d]]$ODF_HEADER$FILE_SPECIFICATION <- paste('MADCPS', '_', obj[['cruise_number']], '_', obj[['mooring_number']], '_', obj[['serialNumber']] , '-', round(obj[['depthMean']] - obj[['distance']][[d]], digits = 0), '.ODF', sep = '')
        
        #CRUISE HEADER
        b[[d]]$CRUISE_HEADER$COUNTRY_INSTITUTE_CODE <- obj[['country_code']]
        b[[d]]$CRUISE_HEADER$CRUISE_NUMBER <- obj[['cruise_number']]
        b[[d]]$CRUISE_HEADER$ORGANIZATION <- obj[['organization']]
        b[[d]]$CRUISE_HEADER$CHIEF_SCIENTIST <- obj[['chief_scientist']]
        b[[d]]$CRUISE_HEADER$START_DATE <- obj[['time_coverage_start']]
        b[[d]]$CRUISE_HEADER$END_DATE <- obj[['time_coverage_end']]
        b[[d]]$CRUISE_HEADER$PLATFORM <- obj[['platform']]
        b[[d]]$CRUISE_HEADER$CRUISE_NAME <- obj[['cruise_name']]
        b[[d]]$CRUISE_HEADER$CRUISE_DESCRIPTION <- obj[['cruise_description']]
        
        
        b[[d]]$EVENT_HEADER$DATA_TYPE <- obj[['data_type']]
        b[[d]]$EVENT_HEADER$EVENT_NUMBER <- obj[['mooring_number']]
        b[[d]]$EVENT_HEADER$EVENT_QUALIFIER1 <- obj[['']]
        b[[d]]$EVENT_HEADER$EVENT_QUALIFIER2 <- obj[['']]
        b[[d]]$EVENT_HEADER$CREATION_DATE <- Sys.Date()
        b[[d]]$EVENT_HEADER$ORIG_CREATION_DATE <- obj[['date_created']]
        b[[d]]$EVENT_HEADER$START_DATE_TIME <- obj[['time_coverage_start']]
        b[[d]]$EVENT_HEADER$END_DATE_TIME <- obj[['time_coverage_end']]
        b[[d]]$EVENT_HEADER$INITIAL_LATITUDE <- obj[['latitude']]
        b[[d]]$EVENT_HEADER$INITIAL_LONGITUDE <- obj[['longitude']]
        b[[d]]$EVENT_HEADER$END_LATITUDE <- obj[['latitude']]
        b[[d]]$EVENT_HEADER$END_LONGITUDE <- obj[['longitude']]
        b[[d]]$EVENT_HEADER$MIN_DEPTH <- obj[['distance']][d] + obj[['depthMean']]
        b[[d]]$EVENT_HEADER$MAX_DEPTH <- obj[['distance']][d] + obj[['depthMean']]    
        b[[d]]$EVENT_HEADER$SAMPLING_INTERVAL <- obj[['sampling_interval']]
        b[[d]]$EVENT_HEADER$SOUNDING <- obj[['sounding']]
        b[[d]]$EVENT_HEADER$DEPTH_OFF_BOTTOM  <- as.numeric(obj[['sounding']]) - obj[['depthMean']]
        b[[d]]$EVENT_HEADER$EVENT_COMMENTS <- obj[['event_comments']]
        
        # INSTRUMENT_HEADER
        
        b[[d]]$INSTRUMENT_HEADER$INST_TYPE <- obj[['instrumentType']]
        b[[d]]$INSTRUMENT_HEADER$MODEL <- obj[['model']]
        b[[d]]$INSTRUMENT_HEADER$SERIAL_NUMBER <- obj[['serialNumber']]
        b[[d]]$INSTRUMENT_HEADER$DESCRIPTION <- obj[['description']]
        
        # RECORD_HEADER
        
        b[[d]]$RECORD_HEADER$NUM_CYCLE <- length(obj[['time']])
        b[[d]]$RECORD_HEADER$NUM_PARAM <- length(params) +1
        
      #delete null headers
  b[[d]]$POLYNOMIAL_CAL_HEADER <- NULL
  b[[d]]$COMPASS_CAL_HEADER <- NULL
  b[[d]]$RECORD_HEADER$NUM_CALIBRATION <- NULL
  b[[d]]$RECORD_HEADER$NUM_SWING <- NULL
      }
  
  
    save(b, file = paste0('MADCPS_', obj[['cruise_number']],'_',  obj[['mooring_number']], '_', obj[['sampling_interval']], '.Rd', sep = ''))
  
      
      #write odf sturctures to odf files
  #doesn't work --- line formatting issue, not skipping to new line
    if (write == TRUE){
    ptm <- proc.time()
    for(d in 1:length(obj[['distance']])){
    write_odf( b[[d]],   output_file =paste0(b[[d]]$ODF_HEADER$FILE_SPECIFICATION))
      print(d)
    }
    proc.time - ptm
    } else{
      return(b)
      print('ODF object ready, please choose bins to export.')
    }
  }
     
  if(inherits(obj, what = 'ctd') ){
    ;
    ;
  }
      
    }
    
    
    
  
####ODF selct bin export####


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
binExport <- function(obj, bins){
  
  
  for(l in bins){
    write_odf( obj[[l]],   output_file =paste0(obj[[l]]$ODF_HEADER$FILE_SPECIFICATION))
    
  }
}


  
  
 
  
