#import package
library(mice)

## PRAPROSES

#load data
data <- read.csv("e:/census-income.csv", header = FALSE, na.strings = " ?")

#eksplorasi
View(data)
summary(data)
str(data)
md.pattern(data)

#isi nama atribut/colom
colnames(data) <- c("age", "class_of_worker","detailed_industry_recode", 
                    "detailed_occupation_recode", "education", "wage_per_hour", 
                    "enroll_in_edu_inst_last_wk", "marital_stat_Never_married", 
                    "major_industry_code", "major_occupation_code", "race", "hispanic_origin", 
                    "sex", "member_of_a_labor_union", "reason_for_unemployment", 
                    "full_or_part_time_employment_stat", "capital_gains", "capital_losses", 
                    "dividends_from_stocks", "tax_filer_stat", "region_of_previous_residence", 
                    "state_of_previous_residence", "detailed_household_and_family_stat", 
                    "detailed_household_summary_in_household", "instance_weight", "migration_code_change_in_msa", 
                    "migration_code_change_in_reg", "migration_code_move_within_reg", "live_in_this_house_1_year_ago", 
                    "migration_prev_res_in_sunbelt", "num_persons_worked_for_employer", 
                    "family_members_under_18", "country_of_birth_father", "country_of_birth_mother", 
                    "country_of_birth_self", "citizenship", "own_business_or_self_employed", 
                    "fill_inc_questionnaire_for_veterans_admin", "veterans_benefits", "weeks_worked_in_year", 
                    "year", "class_gaji")


View(data)

#menghilangkan instance yang lahir di luar US and not citizen of US
new_data <- data[data$citizenship!=" Foreign born- Not a citizen of U S ",]
summary(new_data$citizenship)
new_data$citizenship <- factor(new_data$citizenship)
summary(new_data$citizenship)

#menghilangkan instance yang berumur kurang dari 14 tahun
new_data <- new_data[new_data$age>14,]
summary(new_data$age)

#memberikan nilai string "unknown" pada missing value

for (i in 1:ncol(new_data)) {
  if(sum(is.na(new_data[,i]))>0){
    
    levels <- levels(new_data[,i])
    levels[length(levels)+1] <- "unknown"
    
    new_data[,i] <- factor(new_data[,i], levels = levels)
    new_data[,i][is.na(new_data[,i])] <- "unknown"
  }
}


