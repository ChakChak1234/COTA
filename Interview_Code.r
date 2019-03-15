
# loading libraries
library(survival)
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(survival)
#library(cowplot) # I originally wanted to use cowplot to put them together, but was unsure how it worked in a jupyter environment
library(randomForest)

# load data and convert selected variables into the correct class()
patients_data <- read.csv("simulated_patients.csv", header = TRUE, sep = ",")
patients_data[,8] <- as.POSIXct(strptime(patients_data[,8], "%m/%d/%Y"))
patients_data[,9] <- as.POSIXct(strptime(patients_data[,9], "%m/%d/%Y"))

revenue_data <- read.csv("simulated_revenue.csv", header = TRUE, sep = ",")
revenue_data[,2] <- as.POSIXct(strptime(revenue_data[,2], "%m/%d/%Y"))

# let's look at the two datasets
head(patients_data) 
head(revenue_data)

# filter dataset to include only observations between 01/01/07 to 01/01/12
filt_patients_data <- filter(patients_data, date_of_final_obs > "2012-01-01")

filt_patients_data <- filt_patients_data %>% distinct(patient_identifier, patient_name, sex, ethnicity, age_at_diagnosis, smoking_status, treatment, date_of_diagnosis, date_of_final_obs, final_obs_status)

# let's look at the nrows and ncols
dim(patients_data)
dim(filt_patients_data)

# let's look at the frequency distribution of selected variables
lapply(filt_patients_data[c(-1, -2, -8, -9)], table)

# Used crosstabs to show the gender by ethnicity distribution, and ran chi-square to test for independence of variables
xtabs(~sex+ethnicity, data=filt_patients_data)
sex_by_eth_tbl <- table(filt_patients_data$sex, filt_patients_data$ethnicity)
chisq.test(sex_by_eth_tbl)

# subset dataset by columns age_at_diagnosis and treatment
Q3_data <- filt_patients_data[,c("age_at_diagnosis", "treatment")]
# rename factor levels
levels(Q3_data$treatment) <- list(Treatment=c("Chemotherapy", "Targeted Therapy"), Active_Surveillance=c("Active Surveillance (No Treatment)"))

# Used crosstabs to show the age_at_diagnosis by treatment distribution, and ran chi-square to test for independence of variables
xtabs(~age_at_diagnosis+treatment, data=Q3_data)
age_by_treat_tbl <- table(Q3_data$age_at_diagnosis, Q3_data$treatment)
chisq.test(age_by_treat_tbl, simulate.p.value = TRUE)

# plot histogram to illustrate the two distributions
hist(Q3_data$age_at_diagnosis[with(Q3_data, treatment == "Treatment")], xlim=c(40,110), 
     col="red", main="Treatment vs Active Surveillance (No Treatment) by Age", xlab="Age", ylab="Frequency")
hist(Q3_data$age_at_diagnosis[with(Q3_data, treatment == "Active_Surveillance")], add=T, col="green")

# subset dataset by removing patient id and patient name
Dat <- filt_patients_data[,c(-1,-2)]

# Obtain chi-square distrbution of all combination of variables and return results as table
combos <- combn(ncol(Dat),2)

adply(combos, 2, function(x) {
  test <- chisq.test(Dat[, x[1]], Dat[, x[2]], simulate.p.value = TRUE)

  out <- data.frame("Row" = colnames(Dat)[x[1]]
                    , "Column" = colnames(Dat[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    ,  "df"= test$parameter
                    ,  "p.value" = round(test$p.value, 3)
                    )
  return(out)

})  

# substring function to keep the numeric portion of the alphanumeric elements in patient_id from both datasets
RIGHT = function(x,n){
  substring(x,nchar(x)-n+1)
}

# rename patient_id characters
Q5_patients_data <- filt_patients_data
colnames(Q5_patients_data)[1] <- "patient_id"
Q5_patients_data$patient_id <- as.character(Q5_patients_data$patient_id)
Q5_patients_data$patient_id <- RIGHT(Q5_patients_data$patient_id, 8)

# rename patient_id characters
Q5_revenue_data <- revenue_data[,c(1,3)]
#aggregate charge_amount by patient_id into sum_charge_amount
Q5_revenue_data <- as.data.frame(aggregate(Q5_revenue_data$charge_amount, by=list(Q5_revenue_data$patient_id), FUN=sum))
colnames(Q5_revenue_data) <- c("patient_id", "sum_charge_amount") # rename columns
Q5_revenue_data$patient_id <- as.character(Q5_revenue_data$patient_id)
Q5_revenue_data$patient_id <- RIGHT(Q5_revenue_data$patient_id, 8)

# merge datasets together
merged_filtered_data <- merge.data.frame(Q5_patients_data, Q5_revenue_data, by="patient_id")

# Average amount charged for patients 65 years or older
mean(merged_filtered_data$sum_charge_amount[with(merged_filtered_data, age_at_diagnosis >= 65)])

# filter dataset by patients charged over 100k
over_100k_data <- merged_filtered_data[merged_filtered_data$sum_charge_amount >= 100000, ]
# obtain proportion of those over 65 years of age
nrow(over_100k_data[over_100k_data$age_at_diagnosis >= 65, ])/nrow(over_100k_data)

# subset dataset by filtering for treatment groups that are either chemotherapy or targeted therapy
sub_levels <- c("Chemotherapy","Targeted Therapy")
Q6_filtered_data <- merged_filtered_data[is.element(merged_filtered_data$treatment, sub_levels),]
Q6_filtered_data$treatment <- factor(Q6_filtered_data$treatment)

# run logistic regression
m1 <- glm(treatment ~ sex+ethnicity+age_at_diagnosis+smoking_status+date_of_diagnosis+sum_charge_amount, 
          data = Q6_filtered_data, family = 'binomial') # date_of_final_obs+final_obs_status
summary(m1) # summary output

plot(m1) # plot observed vs expected observations and residuals
Question 6 Answer: Minus patient id and patient name, and date of and status at final observation (assuming physician's choice of therapy was at the date of diagnosis), the model was run on the subsetted dataset to discern what factors, if any, were associated with the physician's choice of targeted therapy vs chemotherapy. Results indicates sex, age at diagnosis, and date of diagnosis as having significant effect on the doctor's choice of therapy, while ethnicity, smoking status, and total charge amount were found to be insignificant. Results, however, should be considered with a caveat given the skewed and kurtotic nature of the distribution in the QQ-plot.
# filter dataset to include patients who expired
Q7_data <- merged_filtered_data[merged_filtered_data$final_obs_status == 'expired', ]

# calculate time difference between date of final observation and date of diagnosis among those who died
Q7_data$time_to_death <- difftime(Q7_data$date_of_final_obs, Q7_data$date_of_diagnosis, unit = "day")

# obtain median time
median(Q7_data$time_to_death) # 2.93634497 years

# create new dataset object for question, obtain time difference variable
Q8_data <- merged_filtered_data
Q8_data$time_to_death <- difftime(Q8_data$date_of_final_obs, Q8_data$date_of_diagnosis, unit = "day")

# fit time_to_death and status into survival curve from survival library to obtain model output
m2 <- survfit(Surv(Q8_data$time_to_death, Q8_data$final_obs_status)~1)
summary(m2) # output summary of survival model

# obtain the summary() of the probability of survival after one year from vector column for all patients
summary(as.data.frame(summary(m2)$pstate)[2])

Q8 Answer: On average, there is a 48% probability that a patient will be alive after 1 year.

# subset dataset by filtering for treatment groups that are either chemotherapy or targeted therapy
sub_levels <- c("Chemotherapy","Targeted Therapy")
# filter dataset
Q9_data <- merged_filtered_data[is.element(merged_filtered_data$treatment, sub_levels),]
# collapses factor levels that are empty
Q9_data$treatment <- factor(Q9_data$treatment)
# preprocess column for survival analysis
Q9_data$final_obs_status_num <- as.numeric(Q9_data$final_obs_status)
# obtain time difference variable
Q9_data$time_to_death <- difftime(Q9_data$date_of_final_obs, Q9_data$date_of_diagnosis)

# obtain and fit cox model of time_to_death and final status by treatment to observed and plot results
m3 <- coxph(Surv(time_to_death, final_obs_status_num) ~ treatment, data = Q9_data) 
temp <- data.frame(treatment=levels(Q9_data$treatment), sex=levels(Q9_data$sex), 
                   age_at_diagnosis=Q9_data$age_at_diagnosis, date_of_diagnosis=Q9_data$date_of_diagnosis)
expfit <- survfit(m3, temp)
plot(expfit, xscale=365.25, ylab="Expected", xlab="Years", main="Survival Rate between Treatment Groups",
     lty = 2:3,col=3:4)
lLab <- gsub("x=","", levels(Q9_data$treatment))  ## legend labels
legend("top", legend=lLab, lty=2:3, col=3:4, horiz=FALSE, bty='n')

# run anova of cox model to compare treatment groups
anova(m3)
summary(m3)
