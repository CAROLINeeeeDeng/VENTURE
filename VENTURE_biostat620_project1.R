# BIOSTAT 620 - Group Project I
# Vikram Bala, Ethan Werner, Neyan Deng
# Team VENTURE

library(readxl)
library(dplyr)
library(ggplot2)

# read data
data <- read_excel("VENTURE_baseline_data.xlsx", sheet="combined_raw_data")
date_range <- seq(as.Date("2024/01/03"), by="day", length.out=42)
data$Date <- c(date_range, date_range, date_range)

# calculate summary statistics
summary_data <- summary(data)
sample_size <- nrow(data)
var_total_st <- var(data$Total.ST.min)
var_pickups <- var(data$Pickups)
var_num_classes <- var(data$Num.Classes)
var_hrs_classes <- var(data$Hrs.Classes)

# generate boxplots
boxplot(data$Total.ST.min, main="Daily Total Screen Time")
boxplot(data$Pickups, main="Daily Number of Pickups")
boxplot(data$Num.Classes, main="Daily Number of Classes")
boxplot(data$Hrs.Classes, main="Daily Hours of Classes")

boxplot(Total.ST.min~ID, data=data, main="Daily Total Screen Time by ID")
boxplot(Pickups~ID, data=data, main="Daily Number of Pickups by ID")
boxplot(Num.Classes~ID, data=data, main="Daily Number of Classes by ID")
boxplot(Hrs.Classes~ID, data=data, main="Daily Hours of Classes by ID")

# generate scatterplots
data_sub <- select(data, "Total.ST.min", "Pickups", "Num.Classes", "Hrs.Classes")
plot(data_sub)
plot(data_sub, col=as.factor(data$ID))

# generate time series plots
ggplot(aes(x=Date, y=Total.ST.min), data=data) + geom_line()
ggplot(aes(x=Date, y=Pickups), data=data) + geom_line()
ggplot(aes(x=Date, y=Num.Classes), data=data) + geom_line()
ggplot(aes(x=Date, y=Hrs.Classes), data=data) + geom_line()

ggplot(aes(x=Date, y=Total.ST.min, color=ID), data=data) + geom_line()
ggplot(aes(x=Date, y=Pickups, color=ID), data=data) + geom_line()
ggplot(aes(x=Date, y=Num.Classes, color=ID), data=data) + geom_line()
ggplot(aes(x=Date, y=Hrs.Classes, color=ID), data=data) + geom_line()
