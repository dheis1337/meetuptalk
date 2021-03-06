library(data.table)
library(microbenchmark)

# planes <- fread("C:/MyStuff/DataScience/Projects/BigData/airOT201201 (2).csv")
planes <- fread("~/MyStuff/DataScience/meetuptalk/airOT201202.csv")

read.data.dt <- microbenchmark(read_data_dt = {
# get files from directory
files <- list.files("~/MyStuff/DataScience/BigData/data/AirOnTimeCSV/", full.names = TRUE)

# read in all files
dt.list <- lapply(files, fread)


# collapse list of data.tables into one list
planes <- rbindlist(dt.list)

}, times = 5, unit = "s")


fread('unzip -p ~/MyStuff/DataScience/Bigdata/data/AirOnTimeCSV/flights_test.csv.zip')

# convert to dat.table
read.data.dt <- data.table(read.data.dt)

# calculate statistics
read.data.dt <- read.data.dt[, list(mean_time = mean(time  / 1000000000),
                                  min_time = min(time  / 1000000000),
                                  median_time = median(time  / 1000000000),
                                  max_time = max(time  / 1000000000)), by = expr]

# write dt
fwrite(read.data.dt, '~/')

change.cols.dt <- microbenchmark("change_col_types_datatable" = {
# change types of columns
# UNIQUE CARRIER, TAIL_NUM, FL_NUM, ORIGIN, DEST, DEST_STATE_ABR, ORIGIN_STATE_ABR,
# DISTANCE_GROUP, CANCELLED, CANCELLATION_CODE, DIVERTED, ARR_DELAY_GROUP, ARR_DEL15,
# DEP_DELAY_GROUP, DEP_DEL15, to factor
planes[, c("UNIQUE_CARRIER", "TAIL_NUM", "FL_NUM", "ORIGIN",
           "DEST", "DEST_STATE_ABR", "ORIGIN_STATE_ABR", "DISTANCE_GROUP", "CANCELLED", 
           "CANCELLATION_CODE", "DIVERTED", "ARR_DELAY_GROUP", "ARR_DEL15", "DEP_DEL15",
           "DEP_DELAY_GROUP") := lapply(.SD, factor),
       .SDcols = c("UNIQUE_CARRIER", "TAIL_NUM", "FL_NUM", "ORIGIN",
                   "DEST", "DEST_STATE_ABR", "ORIGIN_STATE_ABR", "DISTANCE_GROUP", "CANCELLED", 
                   "CANCELLATION_CODE", "DIVERTED", "ARR_DELAY_GROUP", "ARR_DEL15", "DEP_DEL15",
                   "DEP_DELAY_GROUP")]

# CRS_DEP_TIME, DEP_TIME, CRS_ARR_TIME, ARR_TIME
planes[, c("CRS_DEP_TIME", "DEP_TIME", "CRS_ARR_TIME", "ARR_TIME") := lapply(.SD, as.numeric),
        .SDcols = c("CRS_DEP_TIME", "DEP_TIME", "CRS_ARR_TIME", "ARR_TIME")]

}, times = 5, unit = "s")

# create a data.frame
change.cols.dt <- data.table(change.cols.dt)

# calculate statistics 
change.cols.dt <- change.cols.dt[, list(mean_time = mean(time  / 1000000000),
                                  min_time = min(time  / 1000000000),
                                  median_time = median(time  / 1000000000),
                                  max_time = max(time  / 1000000000)), by = expr]

# write to file
fwrite(change.cols.dt, "~/")


add.cols.dt <- microbenchmark(add_cols_dt = { 
# add a few columns
planes[, c("ARR_LATE_FLAG", "ARR_EARLY_FLAG", "DEP_EARLY_FLAG", "DEP_LATE_FLAG",
           "DEP_EARLY_MINS", "ARR_EARLY_MINS", "DEP_DELAY_SQ", "DEP_DELAY_CUBE",
           "DEP_DELAY_QUAR", "DEP_DELAY_DISTANCE", "DEP_DELAY_ELAPSED_TIME", 
           "DEP_DELAY_SQ_DISTANCE", "DEP_DELAY_CUBE_DISTANCE", "DEP_DELAY_QUAR_DISTANCE",
           "ARR_DELAY_SQ", "ARR_DELAY_CUBE" , "ARR_DELAY_QUAR", "ARR_DELAY_DISTANCE",
           "ARR_DELAY_ELAPSED_TIME", "ARR_DELAY_SQ_DISTANCE", "ARR_DELAY_CUBE_DISTANCE",
           "ARR_DELAY_QUAR_DISTANCE") :=
           list(ifelse(ARR_DELAY > 0, 1, 0),
                ifelse(ARR_DELAY <= 0, 1, 0),
                ifelse(DEP_DELAY <= 0, 1, 0),
                ifelse(DEP_DELAY > 0, 1, 0),
                ifelse(DEP_DELAY < 0, abs(DEP_DELAY), NA),
                ifelse(ARR_DELAY < 0, abs(ARR_DELAY), NA),
                DEP_DELAY^2,
                DEP_DELAY^3,
                DEP_DELAY^4,
                DEP_DELAY * DISTANCE,
                DEP_DELAY * ACTUAL_ELAPSED_TIME,
                DEP_DELAY^2 * DISTANCE,
                DEP_DELAY^3 * DISTANCE,
                DEP_DELAY^4 * DISTANCE,
                ARR_DELAY^2,
                ARR_DELAY^3,
                ARR_DELAY^4,
                ARR_DELAY * DISTANCE,
                ARR_DELAY * ACTUAL_ELAPSED_TIME,
                ARR_DELAY^2 * DISTANCE,
                ARR_DELAY^3 * DISTANCE,
                ARR_DELAY^4 * DISTANCE)]



# Add a few  more columns
planes[, c("ARR_EARLY_MINS_DISTANCE", "ARR_EARLY_MINS_ELAPSED_TIME", 
           "ARR_EARLY_MINS_SQ", "ARR_EARLY_MINS_CUBE", "ARR_EARLY_MINS_QUAR",
           "ARR_EARLY_MINS_DEP_DELAY", "ARR_EARLY_MINS_DEP_DELAY_SQ",
           "ARR_EARLY_MINS_DEP_DELAY_CUBE", "ARR_EARLY_MINS_DEP_DELAY_QUAR",
           "DEP_EARLY_MINS_DISTANCE", "DEP_EARLY_MINS_ELAPSED_TIME", 
           "DEP_EARLY_MINS_SQ", "DEP_EARLY_MINS_CUBE", "DEP_EARLY_MINS_QUAR",
           "DEP_EARLY_MINS_ARR_DELAY", "DEP_EARLY_MINS_ARR_DELAY_SQ",
           "DEP_EARLY_MINS_ARR_DELAY_CUBE", "DEP_EARLY_MINS_ARR_DELAY_QUAR") := 
         list(ARR_EARLY_MINS * DISTANCE, 
              ARR_EARLY_MINS * ACTUAL_ELAPSED_TIME,
              ARR_EARLY_MINS^2,
              ARR_EARLY_MINS^3,
              ARR_EARLY_MINS^4,
              ARR_EARLY_MINS * DEP_DELAY,
              ARR_EARLY_MINS * DEP_DELAY_SQ,
              ARR_EARLY_MINS * DEP_DELAY_CUBE,
              ARR_EARLY_MINS * DEP_DELAY_QUAR,
              DEP_EARLY_MINS * DISTANCE, 
              DEP_EARLY_MINS * ACTUAL_ELAPSED_TIME,
              DEP_EARLY_MINS^2,
              DEP_EARLY_MINS^3,
              DEP_EARLY_MINS^4,
              DEP_EARLY_MINS * ARR_DELAY,
              DEP_EARLY_MINS * ARR_DELAY_SQ,
              DEP_EARLY_MINS * ARR_DELAY_CUBE,
              DEP_EARLY_MINS * ARR_DELAY_QUAR)]

}, times = 5, unit = "s")

# create a data.frame
add.cols.dt <- data.table(add.cols.dt)

# calculate statistics 
add.cols.dt <- add.cols.dt[, list(mean_time = mean(time  / 1000000000),
                   min_time = min(time  / 1000000000),
                   median_time = median(time  / 1000000000),
                   max_time = max(time  / 1000000000)), by = expr]

# write to file
fwrite(add.cols.dt, "~/")

