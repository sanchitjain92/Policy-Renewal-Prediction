library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(mice)
library(caret)

setwd("C:/Users/sanch/Downloads/McKinsey Dataset")

##### GENERIC FUNCTIONS ################
#standardize column names
standardize_column_names <- function(col_names){
  #convert to lowercase
  col_names <- tolower(col_names)
  #replace punctuation characters or spaces with underscore
  col_names <- gsub('([[:punct:]])|\\s+','_',col_names)
  #replace double underscore with single underscore
  while (any(grepl("__",col_names,fixed = TRUE)) == TRUE){
    col_names <- gsub("__","_",col_names,fixed = TRUE)
  }
  return(col_names)
}

##### READ DATA ################

raw_training_data <- read_csv("train_ZoGVYWq.csv")
raw_test_data <-  read_csv("test_66516Ee.csv")

colnames(raw_training_data) <- standardize_column_names(names(raw_training_data))
colnames(raw_test_data) <- standardize_column_names(names(raw_test_data))

################### PREPARE DATA ##################

training_data <- raw_training_data %>%
  #convert character fields into factors (this was done after basic check)
  mutate_if(is.character, as.factor) %>%
  #new variables created after basic check
  mutate(age_in_years = age_in_days/365) %>%
  mutate(premium_share = premium/income*100) %>%
  mutate(total_late_payment = count_3_6_months_late +
           count_6_12_months_late +
           count_more_than_12_months_late) %>%
  #remove age in days
  select(-age_in_days)

test_data <- raw_test_data %>%
  #convert character fields into factors (this was done after basic check)
  mutate_if(is.character, as.factor) %>%
  #new variables created after basic check
  mutate(age_in_years = age_in_days/365) %>%
  mutate(premium_share = premium/income*100) %>%
  mutate(total_late_payment = count_3_6_months_late +
           count_6_12_months_late +
           count_more_than_12_months_late) %>%
  #remove age in days
  select(-age_in_days)

#separate numbers and others for better processing during descriptive analysis
training_data_num <- training_data %>% select(-id,-sourcing_channel, -residence_area_type)
training_data_fac <- training_data %>% select(sourcing_channel, residence_area_type)

test_data_num <- test_data %>% select(-id,-sourcing_channel, -residence_area_type)
test_data_fac <- test_data %>% select(sourcing_channel, residence_area_type)

# #### UNIVARIATE ####

#take a glimpse at the data types and data pattern
glimpse(training_data)
glimpse(test_data)

#count distinct entries in each column
distinct_count <- training_data %>%
  summarize_all(funs(n_distinct(.))) %>%
  gather(column, dist_count)

#summary for numeric variable
summary(training_data_num)
summary(test_data_num)

#histogram & boxplot for numeric variables
var_names <- names(training_data_num)
lapply(var_names,function(col_name){
  print(col_name)
  hist(training_data_num[[col_name]])
  boxplot(training_data_num[[col_name]], main = col_name)
})

#freqency count for categorical variables
lapply(training_data_fac, table)
lapply(test_data_fac, table)

#### BI_VARIATE ####

#Bivariate for numeric & numeric
#partial correlation - pearson
library(Hmisc)
partial_corr <- rcorr(as.matrix(training_data_num))
cor_coefficient <- partial_corr$r
p_mat <- partial_corr$P

#correlation plot
library(corrplot)
corrplot(cor_coefficient, method = "square", type = "upper"
         , tl.col = "darkblue"
         , order = "hclust"
         , p.mat = p_mat
         , sig.level = 0.01
         , diag = FALSE
         , insig = "blank"
         , tl.cex = 0.5
)

#bivariate for categorical & categorical

var_names <- c("count_3_6_months_late",
               "count_6_12_months_late",
               "count_more_than_12_months_late",
               "total_late_payment",
               "sourcing_channel",
               "residence_area_type")

bivariate_with_independent_training <- lapply(var_names,function(col_name){
  setNames(as.data.frame(table(training_data[[col_name]], training_data$renewal)),
           c(col_name, "renewal", "count")) %>%
    spread(renewal, count) %>% mutate(renewal_share = round(`1`/(`1` + `0`),3))
})

lapply(var_names,function(col_name){
  setNames(as.data.frame(table(training_data[[col_name]], training_data$renewal)), c(col_name, "renewal", "count")) %>%
    ggplot(aes_string(col_name,"count")) +
    geom_bar(aes(fill = renewal), stat = "Identity", position = "fill")
})



#### MISSING VALUE CHECK ####

var_names <- names(training_data %>% select(-renewal))
#count missing value for each column in training data
missing_count_training <- sapply(var_names, function(col_name){
  sum(is.na(training_data[,col_name]))
})
#count missing value for each column in test data
missing_count_test <- sapply(var_names, function(col_name){
  sum(is.na(test_data[,col_name]))
})

#create data frames of missing values
#1)
missing_values_latepayment_training <- training_data[!complete.cases(training_data %>%
                                                                       select(count_3_6_months_late,
                                                                              count_6_12_months_late,
                                                                              count_more_than_12_months_late)),]

missing_values_latepayment_test <- test_data[!complete.cases(test_data %>%
                                                               select(count_3_6_months_late,
                                                                      count_6_12_months_late,
                                                                      count_more_than_12_months_late)),]

summary(missing_values_latepayment_training)
summary(missing_values_latepayment_test)
# perc_premium_paid_by_cash_credit = 1
# no_of_premium_paid = 2 (most) or 3

#check the overall status of perc_premium_paid_by_cash_credit = 1 & no_of_premiums_paid = 2
lapply(training_data %>%
         filter(perc_premium_paid_by_cash_credit == 1 & no_of_premiums_paid == 2) %>%
         select(count_3_6_months_late, count_6_12_months_late, count_more_than_12_months_late),
       table)

#2)
missing_value_underwriting_training <- training_data[is.na(training_data$application_underwriting_score),]
summary(missing_value_underwriting_training)

missing_value_underwriting_test <- test_data[is.na(test_data$application_underwriting_score),]
summary(missing_value_underwriting_test)


#missing value imputation

#create new variable for missing late payments

training_data_treated_new <- training_data %>%
  mutate(count_3_6_months_late = ifelse(is.na(count_3_6_months_late), 999, count_3_6_months_late)) %>%
  mutate(count_6_12_months_late = ifelse(is.na(count_6_12_months_late), 999, count_6_12_months_late)) %>%
  mutate(count_more_than_12_months_late = ifelse(is.na(count_more_than_12_months_late), 999, count_more_than_12_months_late)) %>%
  mutate(total_late_payment = count_3_6_months_late + count_6_12_months_late + count_more_than_12_months_late) %>%
  mutate(application_underwriting_score = ifelse(is.na(application_underwriting_score),mean(application_underwriting_score,na.rm = TRUE), application_underwriting_score))

test_data_treated_new <- test_data %>%
  mutate(count_3_6_months_late = ifelse(is.na(count_3_6_months_late), 999, count_3_6_months_late)) %>%
  mutate(count_6_12_months_late = ifelse(is.na(count_6_12_months_late), 999, count_6_12_months_late)) %>%
  mutate(count_more_than_12_months_late = ifelse(is.na(count_more_than_12_months_late), 999, count_more_than_12_months_late)) %>%
  mutate(total_late_payment = count_3_6_months_late + count_6_12_months_late + count_more_than_12_months_late) %>%
  mutate(application_underwriting_score = ifelse(is.na(application_underwriting_score),mean(application_underwriting_score,na.rm = TRUE), application_underwriting_score))


############# MODEL ###########################
library(randomForest)
set.seed(1)

sum(is.na(training_data_treated_new))
table(training_data_treated_new$renewal)

x <- training_data_treated_new %>% select(-id, -renewal)
y <- factor(training_data_treated_new$renewal)

tunegrid <- expand.grid(mtry = c(3),
                        ntree = c(1000),
                        sampsize = list(c(4000, 4000), c(3500,3500), c(3000,3000)))

tune.fit.rf.new <- lapply(1:nrow(tunegrid), function(i){
  randomForest(x = x,
               y = y,
               mtry = tunegrid$mtry[i],
               ntree = tunegrid$ntree[i],
               strata = y,
               sampsize = tunegrid$sampsize[[i]])
})

tune.fit.rf.new.oob <- sapply(1:nrow(tunegrid), function(i){
  median(tune.fit.rf.new[[i]]$err.rate[,1])
})
model.fit.rf.new <- tune.fit.rf.new[[which(tune.fit.rf.new.oob == min(tune.fit.rf.new.oob))]]
#confusion matrix
model.fit.rf.new$confusion
#plot
plot(model.fit.rf.new)
#variable importance
importance(model.fit.rf.new, type = 2)
#predict on test data
test_data_treated_new$renewal <- predict(model.fit.rf.new, newdata = test_data_treated_new, type = "prob")
#write output file
output_new <- test_data_treated_new %>% select(id, renewal)

incentives_new <- sapply(1:nrow(output_new),
                         function(i){
                           premium <- raw_test_data$premium[i]
                           incentive <- seq(from = 0, to = 2000, by = 1)
                           effort <- 10*(1 - exp(-incentive/400))
                           improvement <- 20*(1 - exp(-effort/5))/100
                           propensity <- test_data_treated_new$renewal[i]
                           max_revenue = 0
                           max_incentive = 0
                           for (j in 1:length(improvement)) {
                             if (propensity*(1 + improvement[j]) <= 1) {
                               additional_revenue <- improvement[j]*premium - incentive[j]
                               if (max_revenue < additional_revenue) {
                                 max_revenue <- additional_revenue
                                 max_incentive <- incentive[j]
                               }
                             }
                           }
                           max_incentive
                         })
output_new$incentives <- incentives_new
write.csv(output_new, "output_norounding_new_tuned_withincentive_seed1_mtry2_4_newvars2.csv", row.names = FALSE)
