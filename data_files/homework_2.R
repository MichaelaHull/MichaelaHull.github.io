library(jsonlite)
library(dplyr)
library(ggplot2)

setwd('/Users/Aluminum/Documents/Data Visualization')

df <- data.frame(
  state.name,
  state.abb,
  state.x77,
  state.region,
  state.division,
  row.names = NULL
)

names(df) <- c("state", "state_abb", "pop", "income", "illit", "life_exp", "murder", "hs_grad", "frost",
               "area", "region", "division")
df <- mutate(df, type = paste(region, division, sep = " "))

json <- toJSON(
  df,
  dataframe = "rows",
  factor = "string",
  pretty = TRUE
)
cat(json, file = "state.json")

write.csv(df, "state.csv", row.names = FALSE)

# Some EDA
length(unique(df$type))
# Only nine different types, which is the same as the number of divisions

# Looking at individual regions: South, West, Northeast, and North Central
south <- filter(df, region == "South")
west <- filter(df, region == "West")
ne <- filter(df, region == "Northeast")
nc <- filter(df, region == "North Central")
length(unique(south$division)) # 3
length(unique(west$division)) # 2
length(unique(ne$division)) # 2
length(unique(nc$division)) # 2

# Some quick graphs
ggplot() + geom_histogram(data = df, aes(x = region))
ggplot() + geom_histogram(data = df, aes(x = division))
ggplot() + geom_histogram(data = df, aes(x = type))
pairs(df[c(3:10)])
