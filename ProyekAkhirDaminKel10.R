<<<<<<< HEAD
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
=======
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
>>>>>>> c43d7f682b1ec2fd5f38d0d48c575db78ef1ff21
