library(data.table)
library(ggplot2)

# script for creating visualizations for R meetup presentation

# read in dplyr benchmarks
dplyr.bench <- fread('~/MyStuff/DataScience/meetuptalk/dplyrbenchmarks.csv')

# read in data.table benchmarks
dt.bench <- fread('~/MyStuff/DataScience/meetuptalk/datatablebenchmark.csv')

# remove V1 from dplyr.bench
dplyr.bench[, V1 := NULL]

# add a dplyr column to dplyr.bench
dplyr.bench[, package := 'dplyr']

# add a data.table column to dt.bench
dt.bench[, package := 'data.table']

# combind both result data.tables
results <- rbind(dplyr.bench, dt.bench)

# ggplot for sum_byid1
sum.id1 <- results[expr == "sum_by_id1"]

ggplot(sum.id1, aes(x = size, y = mean_time, color = package)) +
  geom_point(size = 3) + 
  geom_line(size = 1.5) +
  ggtitle("Sum of Column v1 by id1") +
  xlab("File Size") +
  ylab("Mean Run Time") +
  theme(plot.title = element_text(hjust = .5)) +
  scale_x_continuous(labels = c("100MB", "500MB", "1GB", "5GB", "10GB"))


# ggplot for sum_by_id1_id2
sum.id1.id2 <- results[expr == "sum_by_id1_id2"]


ggplot(sum.id1.id2, aes(x = size, y = mean_time, color = package)) +
  geom_point(size = 3) + 
  geom_line(size = 1.5) +
  ggtitle("Sum of Column v1 by id1 and id2") +
  xlab("File Size") +
  ylab("Mean Run Time") +
  theme(plot.title = element_text(hjust = .5)) +
  scale_x_continuous(labels = c("100MB", "500MB", "1GB", "5GB", "10GB"))


# ggplot for mean_by_group
mean.group <- results[expr == "mean_by_group"]

ggplot(mean.group, aes(x = size, y = mean_time, color = package)) +
  geom_point(size = 3) +
  geom_line(size = 1.5) + 
  ggtitle("Mean of Columns v1, v2, v3 by id4") +
  xlab("File Size") +
  ylab("Mean Run Time") +
  theme(plot.title = element_text(hjust = .5)) +
  scale_x_continuous(labels = c("100MB", "500MB", "1GB", "5GB", "10GB"))


# sum, mean by id3
sum.mean.id3 <- results[expr == "sum_mean_by_id3"]


ggplot(sum.mean.id3, aes(x = size, y = mean_time, color = package)) +
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  ggtitle("Sum of Column v1, Mean of Column v3 by id1") +
  xlab("File Size") +
  ylab("Mean Run Time") +
  theme(plot.title = element_text(hjust = .5)) +
  scale_x_continuous(labels = c("100MB", "500MB", "1GB", "5GB", "10GB"))



