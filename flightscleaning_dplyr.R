library(dplyr)
library(readr)
library(microbenchmark)

# read in a sample data file to develop with. Will later use more files
planes <- read_csv("~/MyStuff/DataScience/meetuptalk/airOT201202.csv")


read.data.dplyr <- microbenchmark(read_data_dplyr = {
# list files in directory
files <- list.files('~/MyStuff/DataScience/BigData/data/AirOnTimeCSV/', full.names = TRUE)

# lapply read_csv to 
df.list <- lapply(files[1:5], read_csv)

# combine list of data.frames to one datafrae
planes <- do.call(rbind, df.list)

}, times = 5, unit = "s")

read.data.dplyr <- as.data.frame(read.data.dplyr)

# convert nanoseconds time to seconds
read.data.dplyr$time <- read.data.dplyr$time  / 1000000000

read.data.dplyr <- data.frame(expr = read.data.dplyr$expr[1],
                                mean_time = mean(read.data.dplyr$time),
                                min_time = min(read.data.dplyr$time),
                                median_time = median(read.data.dplyr$time),
                                max_time = max(read.data.dplyr$time))


write_csv(read.data.dplyr, "~/MyStuff/DataScience/meetuptalk/read_data_dplyr.csv")


# change types
# UNIQUE CARRIER, TAIL_NUM, FL_NUM, ORIGIN, DEST, DEST_STATE_ABR, ORIGIN_STATE_ABR,
# DISTANCE_GROUP, CANCELLED, CANCELLATION_CODE, DIVERTED, ARR_DELAY_GROUP, ARR_DEL15,
# DEP_DELAY_GROUP, DEP_DEL15, to factor
change.cols.dplyr <- microbenchmark(change_col_types_dplyr = {
planes <- planes %>% mutate_at(.funs = factor, .vars = c("UNIQUE_CARRIER", "TAIL_NUM", "FL_NUM", "ORIGIN",
                                               "DEST", "DEST_STATE_ABR", "ORIGIN_STATE_ABR", "DISTANCE_GROUP", "CANCELLED", 
                                               "CANCELLATION_CODE", "DIVERTED", "ARR_DELAY_GROUP", "ARR_DEL15", "DEP_DEL15",
                                               "DEP_DELAY_GROUP"))


# CRS_DEP_TIME, DEP_TIME, CRS_ARR_TIME, ARR_TIME
planes <- planes %>% mutate_at(.funs = as.numeric, 
                               .vars = c("CRS_DEP_TIME", "DEP_TIME", "CRS_ARR_TIME", "ARR_TIME"))
}, times = 5, unit = "s")

change.cols.dplyr <- as.data.frame(change.cols.dplyr)

# convert nanoseconds time to seconds
change.cols.dplyr$time <- change.cols.dplyr$time  / 1000000000

change.cols.dplyr <- data.frame(expr = change.cols.dplyr$expr[1],
                       mean_time = mean(change.cols.dplyr$time),
                       min_time = min(change.cols.dplyr$time),
                       median_time = median(change.cols.dplyr$time),
                       max_time = max(change.cols.dplyr$time))


write_csv(change.cols.dplyr, "~/MyStuff/DataScience/meetuptalk/change_cols_dplyr.csv")



# add some columns
add.cols.dplyr <- microbenchmark(add_columns = {
planes <- planes %>% 
            mutate(ARR_LATE_FLAG = ifelse(ARR_DELAY > 0, 1, 0),
                   ARR_EARLY_FLAG = ifelse(ARR_DELAY <= 0, 1, 0),
                   DEP_EARLY_FLAG = ifelse(DEP_DELAY <= 0, 1, 0),
                   DEP_LATE_FLAG = ifelse(DEP_DELAY > 0, 1, 0),
                   DEP_EARLY_MINS = ifelse(DEP_DELAY < 0, abs(DEP_DELAY), NA),
                   ARR_EARLY_MINS = ifelse(ARR_DELAY < 0, abs(ARR_DELAY), NA),
                   DEP_DELAY_SQ = DEP_DELAY^2,
                   DEP_DELAY_CUBE = DEP_DELAY^3,
                   DEP_DELAY_QUAR = DEP_DELAY^4,
                   DEP_DELAY_DISTANCE = DEP_DELAY * DISTANCE,
                   DEP_DELAY_ELAPSED_TIME = DEP_DELAY * ACTUAL_ELAPSED_TIME,
                   DEP_DELAY_SQ_DISTANCE = DEP_DELAY^2 * DISTANCE,
                   DEP_DELAY_CUBE_DISTANCE = DEP_DELAY^3 * DISTANCE,
                   DEP_DELAY_QUAR_DISTANCE = DEP_DELAY^4 * DISTANCE,
                   ARR_SQ = ARR_DELAY^2,
                   ARR_CUBE = ARR_DELAY^3,
                   ARR_QUAR = ARR_DELAY^4, 
                   ARR_DELAY_DISTANCE = ARR_DELAY * DISTANCE,
                   ARR_DELAY_ELAPSED_TIME = ARR_DELAY * ACTUAL_ELAPSED_TIME,
                   ARR_DELAY_SQ_DISTANCE = ARR_DELAY^2 * DISTANCE,
                   ARR_DELAY_CUBE_DISTANCE = ARR_DELAY^3 * DISTANCE,
                   ARR_DELAY_QUAR_DISTANCE = ARR_DELAY^4 * DISTANCE)

# add some more columns
planes <- planes %>% 
            mutate(ARR_EARLY_MINS_DISTANCE = ARR_EARLY_MINS * DISTANCE,
                   ARR_EARLY_MINS_ELAPSED_TIME = ARR_EARLY_MINS * ACTUAL_ELAPSED_TIME,
                   ARR_EARLY_MINS_SQ = ARR_EARLY_MINS^2,
                   ARR_EARLY_MINS_CUBE = ARR_EARLY_MINS^3,
                   ARR_EARLY_MINS_QUAR = ARR_EARLY_MINS^4,
                   ARR_EARLY_MINS_DEP_DELAY = ARR_EARLY_MINS * DEP_DELAY,
                   ARR_EARLY_MINS_DEP_DELAY_SQ = ARR_EARLY_MINS * DEP_DELAY_SQ,
                   ARR_EARLY_MINS_DELAY_CUBE = ARR_EARLY_MINS * DEP_DELAY_CUBE,
                   ARR_EARLY_MINS_DEP_DELAY_QUAR = ARR_EARLY_MINS * DEP_DELAY_QUAR,
                   DEP_EARLY_MINS_DISTANCE = DEP_EARLY_MINS * DISTANCE,
                   DEP_EARLY_MINS_ELAPSED_TIME = DEP_EARLY_MINS * ACTUAL_ELAPSED_TIME,
                   DEP_EARLY_MINS_SQ = DEP_EARLY_MINS^2,
                   DEP_EARLY_MINS_CUBE = DEP_EARLY_MINS^3,
                   DEP_EARLY_MINS_QUAR = DEP_EARLY_MINS^4,
                   DEP_EARLY_MINS_ARR_DELAY = DEP_EARLY_MINS ^ ARR_DELAY,
                   DEP_EARLY_MINS_ARR_DELAY_SQ = DEP_EARLY_MINS * ARR_DELAY^2,
                   DEP_EARLY_MINS_ARR_DELAY_CUBE = DEP_EARLY_MINS * ARR_DELAY^3,
                   DEP_EARLY_MINS_ARR_DELAY_QUAR = DEP_EARLY_MINS * ARR_DELAY^4)


}, times = 5, unit = "s")

# convert to a dataframe
add.cols.dplyr <- as.data.frame(add.cols.dplyr)

# convert nanoseconds time to seconds
add.cols.dplyr$time <- add.cols.dplyr$time  / 1000000000

# calculate statistics
add.cols.dplyr <- data.frame(expr = add.cols.dplyr$expr[1],
                                mean_time = mean(add.cols.dplyr$time),
                                min_time = min(add.cols.dplyr$time),
                                median_time = median(add.cols.dplyr$time),
                                max_time = max(add.cols.dplyr$time))

# write file 
write_csv(add.cols.dplyr, "~/MyStuff/DataScience/meetuptalk/add_cols_dplyr.csv")

planes %>% select(-one_of("ARR_TIME"))

