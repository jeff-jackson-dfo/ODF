
#'Functions which allow tranfer between oce objects and ODF files
#'
#'author: Emily Chisholm, emily.chisholm "\@\" dfo-mpo.gc.ca
#'date: June 28 2018
#'
#'
#'
#'
#'
#'
#'
#'
#'
# NULL


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

oce2odf <- function(obj, write = TRUE) {
  
  #identify type of oce object

  if (inherits(obj, what = 'adp')) {
    #file names
    names <- list()
    for (d in 1:length(obj[['distance']])) {
      #caUTION: oce uses snake case, ADCP process uses '_'
      names[[d]] <-
        paste(
          'MADCPS',
          '_',
          obj[['cruise_number']],
          '_',
          obj[['mooring_number']],
          '_',
          obj[['serialNumber']] ,
          '-',
          round(obj[['distance']][[d]], digits = 0),
          '.ODF',
          sep = ''
        )
      
    }
    #name variables to export to ODF
    params <- list('u', 'v', 'w', 'errv', 'pgd', 'agc')
    
    u <- obj[['v']][, , 1]
    v <- obj[['v']][, , 2]
    w <- obj[['v']][, , 3]
    errv <- obj[['v']][, , 4]
    pgd <- obj[['g', 'numeric']][, , 4]
    agc <-
      apply(
        X = obj[['a', 'numeric']],
        MARGIN = 1:2,
        FUN = mean,
        na.rm = TRUE
      )
    #handle time separately
    sytm <- obj[['time']]
    
    #work on add_parameter to make easier
    #split each data variable into single depth time series
    
    #add each section of data as parameter in loop of odf files by depths
    
    #creates data array which matches dimensions of variables,
    b <- NULL
    for (d in 1:length(obj[['distance']])) {
      b[[d]] <- gen_odfstruct()
      b[[d]]$DATA <-
        matrix(NA,  nrow = length(adp[['time']]),  ncol = length(params))
    }
    
    for (i in 1:length(params)) {
      for (d in 1:length(obj[['distance']])) {
        eval(parse(text = paste0(
          "b[[d]]$DATA[,i] <- ", params[[i]], "[,d] "
        )))
        
      }
    }
    
    
    #handle time separately
    for (d in 1:length(obj[['distance']])) {
      for (p in params)
        as.data.frame(b[[d]]$DATA)
      
      colnames(b[[d]]$DATA) <-
        list('EWCT_01',
             'NSCT_01',
             'VCSP_01',
             'ERRV_01',
             'PGDP_01',
             'BEAM_01')
    }
    if (!is.null(obj[['time']])) {
      SYTM_01 <-
        as.character(as.POSIXct(obj[['time']], origin = '1970-01-01 00:00:00'))
      for (d in 1:length(obj[['distance']])) {
        b[[d]]$DATA <- cbind.data.frame(b[[d]]$DATA, SYTM_01)
      }
    }
    
    gf3 <- list()
    for (p in params) {
      gf3[[p]] <- as.gf3(p)
    }
    for (d in 1:length(obj[['distance']])) {
      length(b[[d]]$PARAMETER_HEADER) <-
        length(b[[d]]$PARAMETER_HEADER) + length(params)
      for (i in 1:length(params)) {
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
          MINIMUM_VALUE = as.character(eval(parse(
            text = paste0("min(", params[[i]], ", na.rm = TRUE)")
          ))),
          MAXIMUM_VALUE = as.character(eval(parse(
            text = paste0("max(", params[[i]], ", na.rm = TRUE)")
          ))),
          NUMBER_VALID = as.character(eval(parse(
            text = paste0("length(na.omit(", params[[i]], "[,1]))")
          ))),
          NUMBER_NULL = as.character(eval(parse(
            text = paste0(
              "length(",
              params[[i]],
              "[,1]) - length(na.omit(" ,
              params[[i]],
              "[,1]))"
            )
          )))
        )
      }
      if (!is.null(obj[['time']])) {
        s <- as.gf3('sytm')
        length(b[[d]]$PARAMETER_HEADER) <-
          length(b[[d]]$PARAMETER_HEADER) + 1
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
          MINIMUM_VALUE = toupper(strftime(
            min(as.character(SYTM_01), na.rm = TRUE),
            format = '%d-%b-%Y %T.00',
            tz = "UTC"
          )),
          MAXIMUM_VALUE = toupper(strftime(
            max(as.character(SYTM_01), na.rm = TRUE),
            format = '%d-%b-%Y %T.00',
            tz = "UTC"
          )),
          NUMBER_VALID = length(na.omit(SYTM_01)),
          NUMBER_NULL = length(SYTM_01) - length(na.omit(SYTM_01))
        )
      }
    }
    
    #parameter header, polynomial cal header (optional), compass cal header
    #FIXME: adds to history header with each action like oce processing log
    
    #add header block to each odf file (standard, same for each file)
    
    for (d in 1:length(obj[['distance']])) {
      
      #ODF HEADER
      b[[d]]$ODF_HEADER$FILE_SPECIFICATION <-
        paste('MADCPS',
              '_',
              obj[['cruise_number']],
              '_',
              obj[['mooring_number']],
              '_',
              obj[['serialNumber']] ,
              '-',
              (round(obj[['depthMean']] - obj[['distance']][[d]], digits = 0)),
              sep = '')
      
      #CRUISE HEADER
      b[[d]]$CRUISE_HEADER$COUNTRY_INSTITUTE_CODE <- obj[['country_code']]
      b[[d]]$CRUISE_HEADER$CRUISE_NUMBER <- obj[['cruise_number']]
      b[[d]]$CRUISE_HEADER$ORGANIZATION <- obj[['organization']]
      b[[d]]$CRUISE_HEADER$CHIEF_SCIENTIST <- obj[['chief_scientist']]
      b[[d]]$CRUISE_HEADER$START_DATE <- toupper(strftime(obj[['time_coverage_start']], format = '%d-%b-%Y %T.00', tz = "UTC"))
      b[[d]]$CRUISE_HEADER$END_DATE <- toupper(strftime(obj[['time_coverage_end']], format = '%d-%b-%Y %T.00', tz = "UTC"))
      b[[d]]$CRUISE_HEADER$PLATFORM <- obj[['platform']]
      b[[d]]$CRUISE_HEADER$CRUISE_NAME <- obj[['cruise_name']]
      b[[d]]$CRUISE_HEADER$CRUISE_DESCRIPTION <- obj[['cruise_description']]
      
      #EVENT HEADER
      b[[d]]$EVENT_HEADER$DATA_TYPE <- obj[['data_type']]
      b[[d]]$EVENT_HEADER$EVENT_NUMBER <- obj[['mooring_number']]
      b[[d]]$EVENT_HEADER$EVENT_QUALIFIER1 <- paste(obj[['serialNumber']], '-', round(obj[['depthMean']] - obj[['distance']][[d]], digits = 0))
      b[[d]]$EVENT_HEADER$EVENT_QUALIFIER2 <- obj[['sampling_interval']]
      b[[d]]$EVENT_HEADER$CREATION_DATE <- Sys.Date()
      b[[d]]$EVENT_HEADER$ORIG_CREATION_DATE <- toupper(strftime(Sys.Date(), format = '%d-%b-%Y %T.00', tz = "UTC"))
      b[[d]]$EVENT_HEADER$START_DATE_TIME <- toupper(strftime(obj[['time_coverage_start']], format = '%d-%b-%Y %T.00', tz = "UTC"))
      b[[d]]$EVENT_HEADER$END_DATE_TIME <- toupper(strftime(obj[['time_coverage_end']], format = '%d-%b-%Y %T.00', tz = "UTC"))
      b[[d]]$EVENT_HEADER$INITIAL_LATITUDE <- obj[['latitude']]
      b[[d]]$EVENT_HEADER$INITIAL_LONGITUDE <- obj[['longitude']]
      b[[d]]$EVENT_HEADER$END_LATITUDE <- obj[['latitude']]
      b[[d]]$EVENT_HEADER$END_LONGITUDE <- obj[['longitude']]
      b[[d]]$EVENT_HEADER$MIN_DEPTH <- round(obj[['distance']][d] , digits = 0)
      b[[d]]$EVENT_HEADER$MAX_DEPTH <- round(obj[['distance']][d] , digits = 0)
      b[[d]]$EVENT_HEADER$SAMPLING_INTERVAL <- obj[['sampling_interval']]
      b[[d]]$EVENT_HEADER$SOUNDING <- obj[['sounding']]
      b[[d]]$EVENT_HEADER$DEPTH_OFF_BOTTOM  <- as.numeric(obj[['sounding']]) - obj[['distance']][d]
      b[[d]]$EVENT_HEADER$EVENT_COMMENTS <- paste(as.character(Sys.Date() , obj[['event_comments']]))
      
      # INSTRUMENT_HEADER
      b[[d]]$INSTRUMENT_HEADER$INST_TYPE <- 'ADCP'
      b[[d]]$INSTRUMENT_HEADER$MODEL <- obj[['model']]
      b[[d]]$INSTRUMENT_HEADER$SERIAL_NUMBER <- obj[['serialNumber']]
      b[[d]]$INSTRUMENT_HEADER$DESCRIPTION <- obj[['description']]
      
      # RECORD_HEADER
      b[[d]]$RECORD_HEADER$NUM_CYCLE <- length(obj[['time']])
      b[[d]]$RECORD_HEADER$NUM_PARAM <- length(params) + 1
      
      #delete null headers
      b[[d]]$POLYNOMIAL_CAL_HEADER <- NULL
      b[[d]]$COMPASS_CAL_HEADER <- NULL
      b[[d]]$RECORD_HEADER$NUM_CALIBRATION <- NULL
      b[[d]]$RECORD_HEADER$NUM_SWING <- NULL
    }
    
    save(b, file = paste0('MADCPS_', obj[['cruise_number']], '_',  obj[['mooring_number']], '_', obj[['sampling_interval']], '.Rd', sep = ''))
    
    #write odf sturctures to odf files
    #doesn't work --- line formatting issue, not skipping to new line
    if (write == TRUE) {
      for (d in 1:length(obj[['distance']])) {
        write_odf(b[[d]], output_file = paste0(b[[d]]$ODF_HEADER$FILE_SPECIFICATION, '.ODF'))
        print(paste0("Bin", d, "of", length(obj[['distance']]),  "completed", sep = " "))
      }
      
    } else{
      return(b)
      
    }
  }
  
  ########################
  ####   CTD object   ####
  ########################
  
  if (inherits(obj, what = 'ctd')) {
    
    #name variables to export to ODF
    params <- names(obj@data)
    
    #remove time to handle separately
    t <- grep(params, pattern = "time")
    params <- params[-t]
    
    #creates data array which matches dimensions of variables,
    # b <- NULL
    b <- gen_odfstruct()
    b$DATA <- matrix(NA,  nrow = length(obj[['scan']]),  ncol = length(params))
    
    for (i in 1:length(params)) {
      # Conductivity is in mS/m but needs to be converted to conductivity ratio.
      if (params[[i]] == "conductivity") {
        eval(parse(text = paste0("b$DATA[,i] <- obj[['", params[[i]], "']]/42.914")))
        params[[i]] = "conductivity ratio"
      } else {
        eval(parse(text = paste0("b$DATA[,i] <- obj[['", params[[i]], "']]")))
      }
    }
    
    b$DATA <- as.data.frame(b$DATA)
    
    for (i in 1:length(params)) {
      as.gf3(params[[i]])
    }
    
    colnames(b$DATA) <-
      list('CRAT_01',
           'TEMP_01',
           'PRES_01',
           'OSAT_01',
           'PSAL_01',
           'CNTR_01')
    
    #handle time separately
    if (!is.null(obj[['time']])) {
      SYTM_01 <- as.character(as.POSIXct(obj[['time']], origin = '1970-01-01 00:00:00'))
      b$DATA <- cbind(b$DATA, SYTM_01)
    }

    gf3 <- list()
    for (p in params) {
      gf3[[p]] <- as.gf3(p)
    }
    
    #initialize the number of PARAMETER_HEADERS
    length(b$PARAMETER_HEADER) <- length(b$PARAMETER_HEADER) + length(params)
    
    for (i in 1:length(params)) {
      b$PARAMETER_HEADER[[i]] <- list(
        TYPE = 'DOUB',
        NAME = gf3[[params[[i]]]]$def,
        UNITS = gf3[[i]]$units,
        CODE = paste(gf3[[i]]$code, '01', sep = '_'),
        NULL_VALUE = '-99.0',
        PRINT_FIELD_WIDTH = as.character(gf3[[i]]$width),
        PRINT_DECIMAL_PLACES = as.character(gf3[[i]]$prec),
        ANGLE_OF_SECTION = '-99.0',
        MAGNETIC_VARIATION = '-99.0',
        DEPTH = round(max(b$DATA$PRES_01), digits = 0),
        MINIMUM_VALUE = as.character(eval(parse(text = paste0("format(min(b$DATA[", i, "], na.rm = TRUE), digits = ", gf3[[i]]$prec, ")")))),
        MAXIMUM_VALUE = as.character(eval(parse(text = paste0("format(max(b$DATA[", i, "], na.rm = TRUE), digits = ", gf3[[i]]$prec, ")")))),
        NUMBER_VALID = as.character(eval(parse(text = paste0("length(na.omit(b$DATA[", i, "]))")))),
        NUMBER_NULL = as.character(eval(parse(text = paste0("length(b$DATA[", i, "]) - length(na.omit(b$DATA[" , i, "]))"))))
      )
    }
    
    if (!is.null(obj[['time']])) {
      s <- as.gf3('sytm')
      length(b$PARAMETER_HEADER) <- length(b$PARAMETER_HEADER) + 1
      i <- length(b$PARAMETER_HEADER)
      b$PARAMETER_HEADER[[i]] <- list(
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
        MINIMUM_VALUE = toupper(strftime(min(as.character(SYTM_01), na.rm = TRUE), format = '%d-%b-%Y %T.00', tz = "UTC")),
        MAXIMUM_VALUE = toupper(strftime(max(as.character(SYTM_01), na.rm = TRUE), format = '%d-%b-%Y %T.00', tz = "UTC")),
        NUMBER_VALID = length(na.omit(SYTM_01)),
        NUMBER_NULL = length(SYTM_01) - length(na.omit(SYTM_01))
      )
    }
  }
  
  #parameter header, polynomial cal header (optional), compass cal header
  #FIXME: adds to history header with each action like oce processing log
  
  #add header block to each odf file (standard, same for each file)
  
  #get required information from the user
  # cruise_number <- readline(prompt="Enter Cruise Number: ")
  # event_number <- readline(prompt="Enter Event Number: ")
  # organization <- readline(prompt="Enter Organization: ")
  # platform <- readline(prompt="Enter Platform: ")
  # area_of_operation <- readline(prompt="Enter the Area of Operation: ")
  cruise_number <- "BCD2019669"
  event_number <- "001"
  country_code <- "1810"
  organization <- "DFO SABS"
  chief_scientist <- "FRED PAGE"
  platform <- "VIOLA M DAVIDSON"
  area_of_operation <- "BAY OF FUNDY"
  cruise_description <- "ATLANTIC ZONE MONITORING PROGRAM (AZMP)"
  start_date <- "2019-01-01"
  end_date <- "2019-12-31"
  latitude <- 44.9307
  longitude <- -66.8495
  sounding <- 98.0
  sampling_interval <- 1
  event_comments <- "PRINCE 5 AZMP FIXED STATION"
  inst_model <- "RBRconcerto"
  inst_description <- "RBR Standard Logger"
  
  # Check if file is an upcast or a downcast or both.
  nx <- length(obj[['pressure']])
  pdiff <- obj[['pressure']][nx] - obj[['pressure']][1]
  if (pdiff > 0) {
    castDir <- "DN"
  } else {
    castDir <- "UP"
  }
  
  #ODF HEADER
  b$ODF_HEADER$FILE_SPECIFICATION <- paste('CTD_', cruise_number, '_', event_number, '_', '01', '_', castDir, sep = '')
  
  #CRUISE HEADER
  b$CRUISE_HEADER$COUNTRY_INSTITUTE_CODE <- country_code
  b$CRUISE_HEADER$CRUISE_NUMBER <- cruise_number
  b$CRUISE_HEADER$ORGANIZATION <- organization
  b$CRUISE_HEADER$CHIEF_SCIENTIST <- chief_scientist
  b$CRUISE_HEADER$START_DATE <- toupper(strftime(start_date, format = '%d-%b-%Y %T.00', tz = "UTC"))
  b$CRUISE_HEADER$END_DATE <- toupper(strftime(end_date, format = '%d-%b-%Y %T.00', tz = "UTC"))
  b$CRUISE_HEADER$PLATFORM <- platform
  b$CRUISE_HEADER$CRUISE_NAME <- area_of_operation
  b$CRUISE_HEADER$CRUISE_DESCRIPTION <- cruise_description
  
  #EVENT HEADER
  b$EVENT_HEADER$DATA_TYPE <- "CTD"
  b$EVENT_HEADER$EVENT_NUMBER <- event_number
  b$EVENT_HEADER$EVENT_QUALIFIER1 <- "01"
  b$EVENT_HEADER$EVENT_QUALIFIER2 <- castDir
  b$EVENT_HEADER$CREATION_DATE <- toupper(strftime(Sys.time(), format = '%d-%b-%Y %T.00', tz = "UTC"))
  b$EVENT_HEADER$ORIG_CREATION_DATE <- toupper(strftime(Sys.time(), format = '%d-%b-%Y %T.00', tz = "UTC"))
  b$EVENT_HEADER$START_DATE_TIME <- toupper(strftime(min(obj[['time']]), format = '%d-%b-%Y %T.00', tz = "UTC"))
  b$EVENT_HEADER$END_DATE_TIME <- toupper(strftime(max(obj[['time']]), format = '%d-%b-%Y %T.00', tz = "UTC"))
  b$EVENT_HEADER$INITIAL_LATITUDE <- latitude
  b$EVENT_HEADER$INITIAL_LONGITUDE <- longitude
  b$EVENT_HEADER$END_LATITUDE <- latitude
  b$EVENT_HEADER$END_LONGITUDE <- longitude
  b$EVENT_HEADER$MIN_DEPTH <- round(min(obj[['pressure']]) , digits = 0)
  b$EVENT_HEADER$MAX_DEPTH <- round(max(obj[['pressure']]) , digits = 0)
  b$EVENT_HEADER$SAMPLING_INTERVAL <- sampling_interval
  b$EVENT_HEADER$SOUNDING <- sounding
  if (sounding > 0) {
    b$EVENT_HEADER$DEPTH_OFF_BOTTOM <- sounding - b$EVENT_HEADER$MAX_DEPTH
  } else {
    b$EVENT_HEADER$DEPTH_OFF_BOTTOM <- -99.0
  }
  b$EVENT_HEADER$EVENT_COMMENTS <- event_comments
  
  # INSTRUMENT_HEADER
  b$INSTRUMENT_HEADER$INST_TYPE <- 'CTD'
  b$INSTRUMENT_HEADER$MODEL <- inst_model
  b$INSTRUMENT_HEADER$SERIAL_NUMBER <- obj[['serialNumber']]
  b$INSTRUMENT_HEADER$DESCRIPTION <- inst_description
  
  # RECORD_HEADER
  b$RECORD_HEADER$NUM_CYCLE <- length(obj[['time']])
  b$RECORD_HEADER$NUM_PARAM <- length(params) + 1
  
  #delete null headers
  b$POLYNOMIAL_CAL_HEADER <- NULL
  b$COMPASS_CAL_HEADER <- NULL
  b$RECORD_HEADER$NUM_CALIBRATION <- NULL
  b$RECORD_HEADER$NUM_SWING <- NULL
  
  #save file
  save(b, file = paste0('CTD_', cruise_number, '_', event_number, '_01_', castDir, '.Rd', sep = ''))
  
  #write odf structure to odf file
  #doesn't work --- line formatting issue, not skipping to new line
  if (write == TRUE) {
    write_odf(b, output_file = paste0(b$ODF_HEADER$FILE_SPECIFICATION, '.ODF'))
  } else {
    return(b)
  }
  return(b)
}
  