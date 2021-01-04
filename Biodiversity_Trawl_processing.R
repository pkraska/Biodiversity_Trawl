require(tidyverse)
require(lubridate)
require(geosphere)

# directory of all biodiversity trawl data
files <- list.files("data/BiodiversityTrawl/",
                    full.names = TRUE)

# list all files for each type of csv
set_files     <-
  grep("set", files, value = TRUE, ignore.case = TRUE)
catch_files   <-
  grep("catch", files, value = TRUE, ignore.case = TRUE)
sample_files  <-
  grep("sample", files, value = TRUE, ignore.case = TRUE)

# read in all SET files and cbind them together
# =============================================
# skip_to look for the maximum row number with a leading "," (i.e. blank cell
# in csv) and feeds into the read_csv to skip the header section of the file.
skip_to <- max(grep("^,", readLines(set_files[1])))

# Create variable "merge_set_data" consisting of all the column names
merge_set_data <-
  read_csv(set_files[1],
           skip = skip_to,
           n_max = 0,
           col_types = cols())
set_col_names <- colnames(merge_set_data)

# Loop through all files in the set_files list and read data
for (i in 1:length(set_files)) {
  skip_to <- max(grep("^,", readLines(set_files[i])))
  set_data <-
    read_csv(
      set_files[i],
      skip = 25,
      col_names = set_col_names,
      col_types = cols()
    )
  merge_set_data <- rbind(merge_set_data, set_data)
}

# create good_set_data from merge_set_data and create a unique ID (UID),
# format datetimes, calculate geodetic distance using start/end coordinates,
# and calculate speed in km/h (there is a difference between observed and this
# value!!!)
good_set_data <- merge_set_data %>%
  mutate(
    UID       = paste(MISSION, SETNO, STATION, sep = "."),
    START_SET = as.character(dmy_hms(paste(
      DATE_START, `TIME START`, sep = " "
    ))),
    END_SET   = as.character(dmy_hms(paste(
      DATE_START, `TIME FINISH`, sep = " "
    ))),
    SET_DIST  = distVincentyEllipsoid(
      cbind(.$LATITUDE_START,
            .$LONGITUDE_START),
      cbind(.$LATITUDE_FINISH,
            .$LONGITUDE_FINISH)
    ),
    SET_SECS = as.numeric(difftime(END_SET, START_SET, units = "mins")),
    SET_SPEED = SET_DIST / SET_SECS * 3600 / 1000
  ) %>%
  select(
    MISSION,
    SETNO,
    STATION,
    time_start = START_SET,
    time_finish = END_SET,
    LONGITUDE_START,
    LATITUDE_START,
    LONGITUDE_FINISH,
    LATITUDE_FINISH,
    depth_start_m = DEPTH_START,
    depth_finish_m = DEPTH_FINISH,
    speed_k = SET_SPEED,
    depth_trawl_ave_m = DEPTH_TRWL_AVE,
    temp_trwl_ave_c = TEMP_TRWL_AVE,
    total_time_trawled = SET_SECS
  ) %>%
  rename_with(tolower) %>%
  write_csv("data/output/passamaquoddy_bay_biodiversity_trawl_set_data.csv")


# read in all CATCH files and cbind them together
# ===============================================
# skip_to look for the maximum row number with a leading "," (i.e. blank cell
# in csv) and feeds into the read_csv to skip the header section of the file.
skip_to <- max(grep("^,", readLines(catch_files[1])))

# Create variable "merge_catch_data" consisting of all the column names but no
# data
merge_catch_data <-
  read_csv(catch_files[1],
           skip = skip_to,
           n_max = 0,
           col_types = cols())
catch_col_names <- colnames(merge_catch_data)

# Loop through all files in the catch_files list and read data
for (i in 1:length(catch_files)) {
  skip_to <- max(grep("^,", readLines(catch_files[i])))
  catch_data <-
    read_csv(
      catch_files[i],
      skip = 25,
      col_names = catch_col_names,
      col_types = cols()
    )
  merge_catch_data <- rbind(merge_catch_data, catch_data)
}

# create good_catch_data from merge_catch_data and create a unique ID (UID,
# paste(MISSION, STATION, SETNO, sep = ".")), remove unnecesary columns.
good_catch_data <- merge_catch_data %>%
  mutate(UID = paste(MISSION, SETNO, STATION, sep = ".")) %>%
  select(
    MISSION,
    SETNO,
    STATION,
    SCIENTIFIC_NAME,
    COMMON_NAME,
    abundance = NUMBER_CAUGHT,
    biomass_g = BIOMASS,
    COMMENTS
  ) %>%
  rename_with(tolower) %>%
  write_csv("data/output/passamaquoddy_bay_biodiversity_trawl_catch_data.csv")

# read in all SAMPLE files and cbind them together
# ===============================================
# skip_to look for the maximum row number with a leading "," (i.e. blank cell
# in csv) and feeds into the read_csv to skip the header section of the file.
skip_to <- max(grep("^,", readLines(sample_files[1])))

# Create variable "merge_sample_data" consisting of all the column names but no
# data
merge_sample_data <-
  read_csv(
    sample_files[1],
    skip = skip_to,
    n_max = 0,
    col_types = cols()
  )
sample_col_names <- colnames(merge_sample_data)

# Loop through all files in the sample_files list and read data
for (i in 1:length(sample_files)) {
  skip_to <- max(grep("^,", readLines(sample_files[i])))
  sample_data <-
    read_csv(
      sample_files[i],
      skip = 25,
      col_names = sample_col_names,
      col_types = cols()
    )
  merge_sample_data <- rbind(merge_sample_data, sample_data)
}
good_sample_data <- merge_sample_data %>%
  select(MISSION, SETNO, everything()) %>%
  rename_with(tolower) %>%
  write_csv("data/output/passamaquoddy_bay_biodiversity_trawl_sample_data.csv")
