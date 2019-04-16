library(mice)

data <- read.csv("e:/census-income.csv", header = FALSE, na.strings = " ?")
data
View(data)
summary(data)

str(data$V35)
summary(data$V36)

