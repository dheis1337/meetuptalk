library(data.table)

planes <- fread("C:/MyStuff/DataScience/Projects/BigData/airOT201201 (2).csv")

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



# add a few columns
planes[, c("ARR_LATE_FLAG", "ARR_EARLY_FLAG", "DEP_EARLY_FLAG", "DEP_LATE_FLAG",
           "DEP_EARLY_MINS", "ARR_EARLY_MIN", "DEP_DELAY_SQ", "DEP_DELAY_CUBE",
           "DEP_DELAY_QUAR", "DEP_DELAY_DISTANCE", "DEP_DELAY_ELAPSED_TIME", 
           "DEP_DELAY_SQ_DISTANCE", "DEP_DELAY_CUBE_DISTANCE", "DEP_DELAY_CUBE_DISTANCE",
           "DEP_EARLY_MINS_SQ", "DEP_EARLY_MINS_CUBE", "DEP_EARLY_MINS_QUAR") :=
           list(ifelse(ARR_DELAY_NEW > 0, 1, 0),
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
                DEP_DELAY_NEW^2,
                DEP_DELAY_NEW^3, 
                DEP_DELAY_NEW^4)]


