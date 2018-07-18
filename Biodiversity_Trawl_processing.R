require(tidyverse)
require(lubridate)
require(geosphere)

# directory of all biodiversity trawl data
  files <- list.files("C:/Users/kraskape/Documents/1-Data/AndrewCooper/BiodiversityTrawl/",
                      full.names = TRUE)

# list all files for each type of csv
  set_files     <- grep("set", files, value = TRUE, ignore.case = TRUE)
  catch_files   <- grep("catch", files, value = TRUE, ignore.case = TRUE)
  sample_files  <- grep("sample", files, value = TRUE, ignore.case = TRUE)

# read in all SET files and cbind them together
# =============================================
  # skip_to look for the maximum row number with a leading "," (i.e. blank cell 
  # in csv) and feeds into the read_csv to skip the header section of the file.
  skip_to <- max(grep("^,", readLines(set_files[1])))
  
  # Create variable "merge_set_data" consisting of all the column names
  merge_set_data <- read_csv(set_files[1], skip = skip_to, n_max = 0)
  set_col_names <- colnames(merge_set_data)
  
  # Loop through all files in the set_files list and read data
    for (i in 1:length(set_files)){
      skip_to <- max(grep("^,", readLines(set_files[i])))
     set_data <- read_csv(set_files[i], skip = 25, col_names = set_col_names)
     merge_set_data <- rbind(merge_set_data, set_data)
    }
  
  # create good_set_data from merge_set_data and create a unique ID (UID), 
  # format datetimes, calculate geodetic distance using start/end coordinates, 
  # and calculate speed in km/h
  good_set_data <- merge_set_data %>%
    mutate(UID       = paste(MISSION, SETNO, STATION, sep = "."), 
           START_SET = dmy_hms(paste(DATE_START, `TIME START`, sep = " ")),
           END_SET   = dmy_hms(paste(DATE_START, `TIME FINISH`, sep = " ")),
           SET_DIST  = distVincentyEllipsoid(cbind(.$LATITUDE_START, 
                                                   .$LONGITUDE_START), 
                                             cbind(.$LATITUDE_FINISH, 
                                                   .$LONGITUDE_FINISH)),
           SET_SECS = as.numeric(difftime(END_SET, START_SET, units = "secs")),
           SET_SPEED = SET_DIST/SET_SECS *3600/1000) %>%
    select(UID, START_SET, END_SET, LONGITUDE_START, LATITUDE_START, 
           LONGITUDE_FINISH, LATITUDE_FINISH, DEPTH_START, DEPTH_FINISH, 
           SET_DIST, SET_SECS, SET_SPEED, SPEED, DEPTH_TRWL_AVE, TEMP_TRWL_AVE)
  
  
# read in all CATCH files and cbind them together
# ===============================================
  skip_to <- max(grep("^,", readLines(catch_files[1])))
  