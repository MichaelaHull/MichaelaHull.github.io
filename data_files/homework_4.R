library(ggplot2)
library(dplyr)
library(sqldf)

setwd('/Users/Aluminum/Documents/MichaelaHull.GitHub.io/data_files')

data <- movies
data <- data.frame(data)
data$budget <- as.numeric(as.character(data$budget))
d_ftab <- data.frame(table(data$budget))
data_nona <- filter(data, budget %in% d_ftab$Var1)
data_nona['Category'] <- 'none'
action <- filter(data_nona, Action == 1)
action <- mutate(action, Category = 'Action')
animation <- filter(data_nona, Animation == 1)
animation <- mutate(animation, Category = 'Animation')
comedy <- filter(data_nona, Comedy == 1)
comedy <- mutate(comedy, Category = 'Comedy')
drama <- filter(data_nona, Drama == 1)
drama <- mutate(drama, Category = 'Drama')
doc <- filter(data_nona, Documentary == 1)
doc <- mutate(doc, Category = 'Documentary')
romance <- filter(data_nona, Romance == 1)
romance <- mutate(romance, Category = 'Romance')
short <- filter(data_nona, Short == 1)
short <- mutate(short, Category = 'Short')
data_nona <- rbind(action, animation, comedy, drama, doc, romance, short)
data_nona1 <- filter(data_nona, mpaa != '')
ids <- seq(1, by = 1, length.out = nrow(data_nona))
data_nona['id'] <- ids
write.csv(data_nona, 'movies.csv', row.names=FALSE)

movie_ts <- sqldf("SELECT year, sum(Action), sum(Animation), sum(Comedy), sum(Drama), sum(Documentary),
            sum(Romance), sum(Short), avg(budget), avg(length), avg(rating) FROM data_nona GROUP BY year")
names(movie_ts) <- c('year', 'Action', 'Animation', 'Comedy', 'Drama', 'Documentary', 'Romance', 'Short',
                     'avg_budget', 'avg_length', 'avg_rating')
write.csv(movie_ts, 'movie_ts.csv', row.names=FALSE)