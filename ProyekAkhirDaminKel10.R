#import package
library(mice)

#load data
data <- read.csv("e:/census-income.csv", header = FALSE, na.strings = " ?")
View(data)
summary(data)
md.pattern(data)

str(data$V35)
summary(data$V36)

#menghilangkan instance yang lahir di luar US and not citizen of US
new_data <- data[data$V36!=" Foreign born- Not a citizen of U S ",]

summary(new_data)
View(new_data)
summary(new_data$V42)
