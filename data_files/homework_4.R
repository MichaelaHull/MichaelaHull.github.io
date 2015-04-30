library(ggplot2)
library(dplyr)

setwd('/Users/Aluminum/Documents/MichaelaHull.GitHub.io/data_files')

data <- movies
data <- data.frame(data)
data$budget <- as.numeric(as.character(data$budget))
d_ftab <- data.frame(table(data$budget))
data_nona <- filter(data, budget %in% d_ftab$Var1)
write.csv(data_nona, 'movies.csv', row.names=FALSE)
