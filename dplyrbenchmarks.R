library(dplyr)
library(microbenchmark)



# data sizes
# dat.size <- c(2e6, 2e6 * 5, 2e7, 2e7 *5, 2e8, 2e8 * 5)
dat.size <- c(2e4, 2e5)

micro.list <- vector("list", length = length(dat.size))
for (i in 1:length(dat.size)) {
  N <- dat.size[i]; K = 100
  set.seed(1)
  DF <- data.frame(stringsAsFactors=FALSE,
                   id1 = sample(sprintf("id%03d",1:K), N, TRUE),
                   id2 = sample(sprintf("id%03d",1:K), N, TRUE),
                   id3 = sample(sprintf("id%010d",1:(N/K)), N, TRUE),
                   id4 = sample(K, N, TRUE),                          
                   id5 = sample(K, N, TRUE),                         
                   id6 = sample(N/K, N, TRUE),                       
                   v1 =  sample(5, N, TRUE),                         
                   v2 =  sample(5, N, TRUE),                       
                   v3 =  sample(round(runif(100,max=100),4), N, TRUE)
  )
  
  
  micro <- microbenchmark("sum_by_id1" = {DF %>% group_by(id1) %>% summarise(sum(v1))},
                          "sum_by_id1_id2" = {DF %>% group_by(id1, id2) %>% summarise(sum(v1))},
                          "sum_mean_by_id3" = {DF %>% group_by(id3) %>% summarise(sum(v1), mean(v3))},
                          "mean_by_group" = {DF %>% group_by(id4) %>% summarise_at(funs(mean), .vars = c("v1", "v2", "v3"))},
                          "counts_by_id1" = {DF %>% group_by(id1) %>% count()},
                          times = 5, unit = "s")

  
micro.df <- as.data.frame(micro)
  
  
micro.df <- micro.df %>% group_by(expr) %>% summarise("min_time" = min(time / 1000000000),
                                                        "mean_time" = mean(time / 1000000000),
                                                        "median_time" = median(time / 1000000000), 
                                                        "max_time" = max(time / 1000000000))
  
  micro.df$size <- dat.size[i]
  
  micro.list[[i]] <- micro.df

}


micro.df <- do.call(rbind, micro.list)

write.csv(micro.df, "~/MyStuff/DataScience/meetuptalk/dplyrbenchmarks.csv",
          row.names = FALSE)



