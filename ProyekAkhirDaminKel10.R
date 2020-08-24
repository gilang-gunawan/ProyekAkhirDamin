#import package
library(party)
library(mice)
library(ggplot2)
library(rpart)
library(MASS)
library(usdm)
nrow(data)
## PRAPROSES

#load data
data <- read.csv("e:/census-income.csv", header = FALSE, na.strings = " ?")

#eksplorasi
nrow(data)
View(data)
summary(data)
str(data)

md.pattern(data)

#isi nama atribut/colom
colnames(data) <- c("age", "class_worker", "det_ind_code", 
                    "det_occ_code", "education", "wage_per_hour", 
                    "hs_college", "marital_stat", 
                    "major_ind_code", "major_occ_code", "race", "hisp_origin", 
                    "sex", "union_member", "unemp_reason", "full_or_part_emp", 
                    "capital_gains", "capital_losses", "stock_dividends", 
                    "tax_filer_stat", "region_prev_res", "state_prev_res", 
                    "det_hh_fam_stat", "det_hh_summ", "instance_weight", 
                    "mig_chg_msa", "mig_chg_reg", 
                    "mig_move_reg", "mig_same", "mig_prev_sunbelt", "num_emp", 
                    "fam_under_18", "country_father", "country_mother", "country_self", 
                    "citizenship", "own_or_self", "vet_question", "vet_benefits", 
                    "weeks_worked", "year", "class_income"
)


View(data)
str(data)

#menghilangkan instance yang lahir di luar US and not citizen of US
summary(data$citizenship)
new_data <- data[data$citizenship!=" Foreign born- Not a citizen of U S ",]
summary(new_data$citizenship)
new_data$citizenship <- factor(new_data$citizenship)
summary(new_data$citizenship)
boxplot(new_data$age)

#menghilangkan instance yang berumur kurang dari 14 tahun
boxplot(det_occ_code ~ major_occ_code, data=data, col="yellow",xlab="class worker", ylab="Age")
summary(new_data$major_occ_code)
summary(new_data$class_worker)
hist(data$age)

new_data <- new_data[new_data$age>14,]
summary(new_data$age)
summary(new_data)
hist(new_data$age)

#memberikan nilai string "unknown" pada missing value
cat(sprintf("\"%s\" \"%s\"\n", df$r, df$interest))

for (i in 1:ncol(new_data)) {
  if(sum(is.na(new_data[,i]))>0){
    #print(names(new_data[i]), sum(is.na(new_data[i])))
    cat(sprintf("\"%s\" \"%s\"\n",names(new_data[i]) , sum(is.na(new_data[i]))))
    levels <- levels(new_data[,i])
    levels[length(levels)+1] <- "unknown"
    
    new_data[,i] <- factor(new_data[,i], levels = levels)
    new_data[,i][is.na(new_data[,i])] <- "unknown"
  }
}

md.pattern(new_data)

#bagi data
set.seed(1234)
indx <- sample(2, nrow(new_data), replace=TRUE, prob=c(0.8, 0.2))
train_data <- new_data[indx==1,]
test_data <- new_data[indx==2,]
train_label <- factor(train_data[,"class_income"])
str(train_data)
nrow(train_data)
nrow(test_data)
#party

library(party) 
myFormula <- class_income ~.
train_ctree <- ctree(myFormula, data=train_data)
print(train_ctree)
plot(train_ctree)
plot(train_ctree, type = "simple")

testPred <- predict(train_ctree, newdata = test_data) 
summary(test_data)
tabel_ctree <- table(testPred, test_data$class_income) 
tabel_ctree
akurasi <- (tabel_ctree[1,1]+tabel_ctree[2,2])/(nrow(test_data))
akurasi*100


#test rpart
myFormula <- class_income ~.
train_rpart <- rpart(myFormula, data = train_data, control = rpart.control(minsplit = 100))
print(train_rpart)
plot(train_rpart)
text(train_rpart, use.n = TRUE)
rparttrain <- predict(train_rpart, newdata = train_data, type = "class")


test_pred <- predict(train_rpart, newdata = test_data, type = "class") 
test_pred

tabel <- table(test_pred, test_data$class_income)

tabel

akurasi <- (tabel[1,1]+tabel[2,2])/(nrow(test_data))
akurasi*100
