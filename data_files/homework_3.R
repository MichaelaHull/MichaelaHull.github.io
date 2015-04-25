setwd('/Users/Aluminum/Documents/Data Visualization')

ts_data <- Seatbelts
df_data <- data.frame(ts_data)
t <- seq(as.Date("1969/1/1"), by = "month", length.out = 192)
id <- seq(from = 1, to = 192, by = 1)
id <- as.character(id)
df_data <- cbind(id, t, df_data)
names(df_data) <- c("id","date", "killed", "drivers", "front", "rear", "kms", "petrol", "van_killed",
                    "law")

write.csv(df_data, 'drivers.csv', row.names = FALSE)

