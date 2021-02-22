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


oce2odfHeader <- function(obj) {
  b <- gen_odfstruct()
  
  if (inherits(obj) == 'adp') {
    # for ( d in 1:length(obj[['distance']])){
    # b$ODF_HEADER$FILE_SPECIFICATION <- paste('MADCPS', '_', obj[['cruise_number']], '_', obj[['mooring_number']], '_', obj[['serial_number']] , '-', obj[['sensor_depth']] - obj[['distance']][[d]], '.ODF', sep = '')
    # }
    b$CRUISE_HEADER$COUNTRY_INSTITUTE_CODE <- obj[['country_code']]
    b$CRUISE_HEADER$CRUISE_NUMBER <- obj[['cruise_number']]
    b$CRUISE_HEADER$ORGANIZATION <- obj[['organization']]
    b$CRUISE_HEADER$CHIEF_SCIENTIST <- obj[['chief_scientist']]
    b$CRUISE_HEADER$START_DATE <- obj[['time_coverage_start']]
    b$CRUISE_HEADER$END_DATE <- obj[['time_coverage_end']]
    b$CRUISE_HEADER$PLATFORM <- obj[['platform']]
    b$CRUISE_HEADER$CRUISE_NAME <- obj[['cruise_name']]
    b$CRUISE_HEADER$CRUISE_DESCRIPTION <-
      obj[['cruise_description']]
    
    
    b$EVENT_HEADER$DATA_TYPE <- obj[['data_type']]
    b$EVENT_HEADER$EVENT_NUMBER <- obj[['mooring_number']]
    b$EVENT_HEADER$EVENT_QUALIFIER1 <- ''
    b$EVENT_HEADER$EVENT_QUALIFIER2 <- ''
    b$EVENT_HEADER$CREATION_DATE <- Sys.Date()
    b$EVENT_HEADER$ORIG_CREATION_DATE <- obj[['date_created']]
    b$EVENT_HEADER$START_DATE_TIME <- obj[['time_coverage_start']]
    b$EVENT_HEADER$END_DATE_TIME <- obj[['time_coverage_end']]
    b$EVENT_HEADER$INITIAL_LATITUDE <- obj[['latitude']]
    b$EVENT_HEADER$INITIAL_LONGITUDE <- obj[['longitude']]
    b$EVENT_HEADER$END_LATITUDE <- obj[['latitude']]
    b$EVENT_HEADER$END_LONGITUDE <- obj[['longitude']]
    b$EVENT_HEADER$MIN_DEPTH <-
      min(obj[['depth']])     #CAUTION THESE ARE ONLY PLACEHOLDERS, EACH FILE SHOULD HAVE
    b$EVENT_HEADER$MAX_DEPTH <-
      max(obj[['depth']])     #THESE VALUES REPLACED BY BIN DEPTH IN THE OCE2ODF FUNCTION
    b$EVENT_HEADER$SAMPLING_INTERVAL <- obj[['sampling_interval']]
    b$EVENT_HEADER$SOUNDING <- obj[['sounding']]
    b$EVENT_HEADER$DEPTH_OFF_BOTTOM  <-
      max(obj[['depth']]) - obj[['depthMean']]
    b$EVENT_HEADER$EVENT_COMMENTS <- obj[['event_comments']]
    
    
    b$INSTRUMENT_HEADER$INST_TYPE <- obj[['instrumentType']]
    b$INSTRUMENT_HEADER$MODEL <- obj[['model']]
    b$INSTRUMENT_HEADER$SERIAL_NUMBER <- obj[['serialNumber']]
    b$INSTRUMENT_HEADER$DESCRIPTION <- obj[['description']]
    
    return(b)
    
    
    
  }
}
