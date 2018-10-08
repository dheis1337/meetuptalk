library(dplyr)

# read in a sample data file to develop with. Will later use more files
planes <- fread("~/MyStuff/DataScience/meetuptalk/airOT201202.csv")

# change types
# UNIQUE CARRIER, TAIL_NUM, FL_NUM, ORIGIN, DEST, DEST_STATE_ABR, ORIGIN_STATE_ABR,
# DISTANCE_GROUP, CANCELLED, CANCELLATION_CODE, DIVERTED, ARR_DELAY_GROUP, ARR_DEL15,
# DEP_DELAY_GROUP, DEP_DEL15, to factor
planes <- planes %>% mutate_at(.funs = factor, .vars = c("UNIQUE_CARRIER", "TAIL_NUM", "FL_NUM", "ORIGIN",
                                               "DEST", "DEST_STATE_ABR", "ORIGIN_STATE_ABR", "DISTANCE_GROUP", "CANCELLED", 
                                               "CANCELLATION_CODE", "DIVERTED", "ARR_DELAY_GROUP", "ARR_DEL15", "DEP_DEL15",
                                               "DEP_DELAY_GROUP"))


# CRS_DEP_TIME, DEP_TIME, CRS_ARR_TIME, ARR_TIME
planes <- planes %>% mutate_at(.funs = as.numeric, 
                               .vars = c("UNIQUE_CARRIER", "TAIL_NUM", "FL_NUM", "ORIGIN",
                                         "DEST", "DEST_STATE_ABR", "ORIGIN_STATE_ABR", 
                                         "DISTANCE_GROUP", "CANCELLED", "CANCELLATION_CODE",
                                         "DIVERTED", "ARR_DELAY_GROUP", "ARR_DEL15", "DEP_DEL15",
                                         "DEP_DELAY_GROUP"))

# add some columns
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


