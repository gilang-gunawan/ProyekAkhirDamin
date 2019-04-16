#required package
library(mice)
#load csv, renamed as data
data <- read.csv("census-income.csv", header = FALSE, na.strings = " ?")
data


View(data)
summary(data)
#statistika ringkasan
str(data$V35)
summary(data$V36)
#cleaning dataset
data[data=="?"] <- NA
#menghilangkan missing value
data <- na.omit(data)
