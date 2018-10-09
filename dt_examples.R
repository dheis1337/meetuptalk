library(data.table)

# create a random data.table
N <- 2e5; K = 100
set.seed(1)
DT <- data.table(
  id1 = sample(sprintf("id%03d",1:K), N, TRUE),      # large groups (char)
  id2 = sample(sprintf("id%03d",1:K), N, TRUE),      # large groups (char)
  id3 = sample(sprintf("id%010d",1:(N/K)), N, TRUE), # small groups (char)
  id4 = sample(K, N, TRUE),                          # large groups (int)
  id5 = sample(K, N, TRUE),                          # large groups (int)
  id6 = sample(N/K, N, TRUE),                        # small groups (int)
  v1 =  sample(5, N, TRUE),                          # int in range [1,5]
  v2 =  sample(5, N, TRUE),                          # int in range [1,5]
  v3 =  sample(round(runif(100,max=100),4), N, TRUE) # numeric e.g. 23.5749
)  

# working in i
# get all rows with corresponding id1 and id3 values
DT[id1 == 'id027']
DT[id1 == 'id027' & id2 == 'id079']

# Get the first two rows
DT[1:5]

# Reordering, id1 descending, id2 ascending. Uses data.table internal forder (radix sort). 
# Operation was so fast it was eventually adopted into base R as of 2016 in R 3.3.0
DT[order(-id1, id2)]


# working in j
# select columns two ways
DT[, .(id1)] # returns data.table
DT[, id1] # returns vector

# computing in j
DT[, sum(v1)] # single computation
DT[, .(sum_v1 = sum(v1), mean_v3 = mean(v3))] # multiple computations
DT[, .(v1 = v1, sd_v3 = sd(v3))] # typical R recyling

# add column
DT[, v4 := v3^2]

# add multiple columns
DT[, ':=' 
      (v5 = v2^2, # square v2
       v6 = v2^3)] # square v3
      
# another way to add multiple columns
DT[, c("v7", "v8") := 
      list(round(v3),
           round(v4))]

# Group by in J
# group by one group
DT[, mean(v3), by = id1]

# group by two groups
DT[, mean(v3), by = .(id1, id2)]

# counts by group
DT[, .N, by = id1]

planes %>% group_by(DAY_OF_WEEK, DAY_OF_MONTH) %>%
  summarise(mean(DEP_DELAY, na.rm = TRUE))


