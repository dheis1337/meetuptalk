

library(data.table)
library(microbenchmark)

# data sizes
# dat.size <- c(2e6, 2e6 * 5, 2e7, 2e7 *5, 2e8, 2e8 * 5)

test.size <- c(2e2, 2e3)

micro.list <- vector("list", length = length(test.size))
for (i in 1:length(test.size)) {
  N <- test.size[i]; K = 10
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
  
  
  micro <- microbenchmark("sum_by_id1" = {DT[, sum(v1), keyby = id1]},
                          "sum_by_id1_id2" = {DT[, sum(v1), keyby = "id1,id2"]},
                          "sum_mean_by_id3" = {DT[, list(sum(v1), mean(v3), keyby = id3)]},
                          "mean_by_group" = {DT[, lapply(.SD, mean), keyby = id4, .SDcols = 7:9]},
                          "counts_by_id1" = {DT[, .N, by = id1]},
                          times = 5, unit = "s")
  
  
  
  micro.dt <- as.data.table(micro)
  
  
  micro.calc <- micro.dt[, list(
    min_time = min(time / 1000000000),
    mean_time = mean(time / 1000000000 ),
    median_time = median(time / 1000000000),
    max_time = max(time / 1000000000)),
    by = expr]
  
  micro.calc[, size := test.size[i]]
  micro.list[[i]] <- micro.calc
  names(micro.list[i]) <- as.character(test.size[i])
  
}

final.results <- do.call(rbind, micro.list)

fwrite(final.results, "~/datatablebenchmark.csv")


