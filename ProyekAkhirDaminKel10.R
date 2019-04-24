#import package
library(mice)

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

#menghilangkan instance yang berumur kurang dari 14 tahun
new_data <- new_data[new_data$age>14,]
summary(new_data$age)


#memberikan nilai pada missing value
summary(new_data)
View(new_data)
str(new_data)

#memberi nilai na pada country_of_birth_father menjadi unknown
summary(new_data$country_of_birth_father) #ada 4085 dara NA
levels <- levels(new_data$country_of_birth_father)
levels[length(levels)+1] <- "unknown"

new_data$country_of_birth_father <- factor(new_data$country_of_birth_father, levels = levels)
new_data$country_of_birth_father[is.na(new_data$country_of_birth_father)] <- "unknown"

summary(new_data$country_of_birth_father)

#memberi nilai na pada country_of_birth_mother menjadi unknown
summary(new_data$country_of_birth_mother) #ada 4085 dara NA
levels <- levels(new_data$country_of_birth_mother)
levels[length(levels)+1] <- "unknown"

new_data$country_of_birth_mother <- factor(new_data$country_of_birth_mother, levels = levels)
new_data$country_of_birth_mother[is.na(new_data$country_of_birth_mother)] <- "unknown"

summary(new_data$country_of_birth_mother)

#memberi nilai na pada country_of_birth_self menjadi unknown
summary(new_data$country_of_birth_self) #ada 4085 dara NA
levels <- levels(new_data$country_of_birth_self)
levels[length(levels)+1] <- "unknown"

new_data$country_of_birth_self <- factor(new_data$country_of_birth_self, levels = levels)
new_data$country_of_birth_self[is.na(new_data$country_of_birth_self)] <- "unknown"

summary(new_data$country_of_birth_self)

#mengisi nilai na pada migration_prev_res_in_sunbelt menjadi unknown
summary(new_data$migration_prev_res_in_sunbelt) #ada 4085 dara NA
levels <- levels(new_data$migration_prev_res_in_sunbelt)
levels[length(levels)+1] <- "unknown"

new_data$migration_prev_res_in_sunbelt <- factor(new_data$migration_prev_res_in_sunbelt, levels = levels)
new_data$migration_prev_res_in_sunbelt[is.na(new_data$migration_prev_res_in_sunbelt)] <- "unknown"

summary(new_data$migration_prev_res_in_sunbelt)

# isi nilai na migration_code-move_within_reg menjadi unknown
summary(new_data$migration_code_move_within_reg)
levels <- levels(new_data$migration_code_move_within_reg)
levels[length(levels)+1] <- "unknown"

new_data$migration_code_move_within_reg <- factor(new_data$migration_code_move_within_reg, levels = levels)
new_data$migration_code_move_within_reg[is.na(new_data$migration_code_move_within_reg)] <- "unknown"

summary(new_data$migration_code_move_within_reg)

# isi nilai na pada migration_code-change_in_msa menjadi unknown
summary(new_data$migration_code_change_in_msa)
levels <- levels(new_data$migration_code_change_in_msa)
levels[length(levels)+1] <- "unknown"

new_data$migration_code_change_in_msa <- factor(new_data$migration_code_change_in_msa, levels = levels)
new_data$migration_code_change_in_msa[is.na(new_data$migration_code_change_in_msa)] <- "unknown"

summary(new_data$migration_code_change_in_msa)

# isi nilai na pada migration_code-change_in_reg mnjd unknown
summary(new_data$migration_code_change_in_reg)
levels <- levels(new_data$migration_code_change_in_reg)
levels[length(levels)+1] <- "unknown"

new_data$migration_code_change_in_reg <- factor(new_data$migration_code_change_in_reg, levels = levels)
new_data$migration_code_change_in_reg[is.na(new_data$migration_code_change_in_reg)] <- "unknown"

summary(new_data$migration_code_change_in_reg)

# isi nilai na pada state_of_previous_residence mnjd unknown
summary(new_data$state_of_previous_residence)
levels <- levels(new_data$state_of_previous_residence)
levels[length(levels)+1] <- "unknown"

new_data$state_of_previous_residence <- factor(new_data$state_of_previous_residence, levels = levels)
new_data$state_of_previous_residence[is.na(new_data$state_of_previous_residence)] <- "unknown"

summary(new_data$state_of_previous_residence)

