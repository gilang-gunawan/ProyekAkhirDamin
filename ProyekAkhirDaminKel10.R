library(mice)

data <- read.csv("e:/census-income.csv", header = FALSE, na.strings = " ?")
data
View(data)
summary(data)

new_data <- na.omit(data)
summary(new_data)

md.pattern(new_data)

str(new_data)
summary(new_data$V34)

data_us <- 