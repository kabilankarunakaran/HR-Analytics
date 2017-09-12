
#******************************************************************************************************************************************************
############################################################### HR ANALYTICS CASE STUDY SOLUTION  #######################################################
#******************************************************************************************************************************************************

#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation

#******************************************************************************************************************************************************
########################################################################## STEP 1: BUSINESS UNDERSTANDING  #######################################################
#******************************************************************************************************************************************************

#The comapnay XYZ has recorded its employee's informations like 
#1. Work experience related info 
#2. Job satisfaction realted info
#3. Attendance info and other factors
#4. And also  has the attrition rate details 

## AIM:

#The company aims  automate the process of predicting 
# if a employee would stay or not and to find the factors that act as predictors for attrition . 

#******************************************************************************************************************************************************
########################################################################## STEP 2: DATA UNDERSTANDING  #######################################################
#******************************************************************************************************************************************************

#Intsall required packages 
install.packages("caTools")
install.packages("stringr")
install.packages("MASS")
install.packages("dplyr")
install.packages("ROCR")
install.packages("ggplot2")
install.packages("car")
install.packages("reshape2")
#Set wrking directory
setwd("F:\\UPGRAD\\My_Solutions\\PA-I_Case_Study_HR_Analytics")

#Load library

library(ROCR)
library(stringr)
library(dplyr)
library(MASS)
library(car)
library(caTools)
library(reshape2)
library(ggplot2)
library(e1071)

# Load Data 
emp_sur_data <- read.csv("employee_survey_data.csv")
general_data <- read.csv("general_data.csv")
in_time <- read.csv("in_time.csv",check.names = FALSE)
out_time <- read.csv("out_time.csv",check.names = FALSE)
mgr_sur_data <- read.csv("manager_survey_data.csv")


str(emp_sur_data)             #4410 obs. of  4 variables:
str(mgr_sur_data)             #4410 obs. of  3 variables:        
str(general_data)             #4410 obs. of  24 variables:
str(in_time)                  #4410 obs. of  262 variables:
str(out_time)                 #4410 obs. of  262 variables:


#on observing the values of NumCompaniesWorked,  there are few records  for which NumCompaniesWorked is zero but Totalwrking experience has value greater than zero 
#and several other inconsistent observations were noticed, to address those, the below assumption is made

#ASSUMPTION: NumCompaniesWorkde denotes count of total no of companies the employee worked including the current company
#As per this assumption, changes were done in the values of NumCopanies Worked column.

table(general_data$NumCompaniesWorked)
table(general_data$TotalWorkingYears)
table(general_data$YearsAtCompany)


general_data$NumCompaniesWorked[which((general_data$TotalWorkingYears==general_data$YearsAtCompany)
                                      & (general_data$NumCompaniesWorked==0 | general_data$NumCompaniesWorked==1))] <- 1

general_data$NumCompaniesWorked[which((general_data$TotalWorkingYears > general_data$YearsAtCompany)
                                      & (general_data$NumCompaniesWorked==0 | general_data$NumCompaniesWorked==1))] <- 2




#Before binding all the df, required variables are converted to factor as per data dictionary 
emp_sur_data$EnvironmentSatisfaction <- as.factor(emp_sur_data$EnvironmentSatisfaction)
emp_sur_data$JobSatisfaction <- as.factor(emp_sur_data$JobSatisfaction)
emp_sur_data$WorkLifeBalance <- as.factor(emp_sur_data$WorkLifeBalance)
mgr_sur_data$JobInvolvement <- as.factor(mgr_sur_data$JobInvolvement)
mgr_sur_data$PerformanceRating <- as.factor(mgr_sur_data$PerformanceRating)

#before merging checking for diff in employee id between two df
setdiff(general_data$EmployeeID,emp_sur_data$EmployeeID)
setdiff(general_data$EmployeeID,mgr_sur_data$EmployeeID)

#merging general, employeee survey and manager survey data 
master_data <- merge(general_data,emp_sur_data,by="EmployeeID")
master_data <- merge(master_data,mgr_sur_data,by="EmployeeID")
#View(master_data)

#******************************************************************************************************************************************************
########################################################################## STEP 3: DATA PRPARATION AND EDA  #######################################################
#******************************************************************************************************************************************************

###################################  Creating Derived Columns like "wrk_hrs_percent" and "el_ot"###################################

#wrk_hrs_percentage     ==> average of percentage of hours worked by an employee
#el_ot                  ==> denotes whether employee is "Early leaver" or "over time worked"

#Handling intime and outtime to find average wrking hours and then average wrking hours will be used t find wrk_hrs_percent
in_col_names <- colnames(in_time)
out_col_names <- colnames(out_time)
in_col_names
out_col_names

#function to convert the in_time and out_time to date format 
convert_todateime <- function(x){
  res <- strptime(x = as.character(x), format = "%Y-%m-%d %H:%M:%S")
  return(res)
}

#converting in and out time to date format by invoking function created 
in_time.date <- as.data.frame(apply(in_time,2,convert_todateime))
out_time.date <- as.data.frame(apply(out_time,2,convert_todateime))

#as the colnames are not proper, provinding proper colnames
colnames(in_time.date) <- in_col_names
colnames(out_time.date) <- out_col_names

#removing columns which has only NA  from in and out
in_time.date <- in_time.date[, !apply(is.na(in_time.date), 2, all)]
out_time.date <- out_time.date[, !apply(is.na(out_time.date), 2, all)]
#View(in_time.date)
#View(out_time.date)

#Finding average hours 
wrk_hours <- as.data.frame(out_time.date - in_time.date)
wrk_hours
wrk_hours.num <- apply(wrk_hours,2,as.character)
wrk_hours.num <- as.numeric(str_replace_all(wrk_hours.num,pattern =" hours",replacement = ""))
wrk_hours.num <- matrix(wrk_hours.num,nrow = 4410, ncol = 249, byrow = FALSE)
wrk_hours.num <- as.data.frame(wrk_hours.num)
#View(wrk_hours.num)

avg_hours <- as.data.frame(apply(wrk_hours.num,1,mean,na.rm=TRUE))
colnames(avg_hours) <- c("avg_wrk_hours")
#View(avg_hours)


#binding avg_wrk_hrs col to master df and using it to find yearly work hours percenatage 
master_data <- cbind(master_data,avg_hours)
master_data$Education <- as.factor(master_data$Education)
master_data$JobLevel <- as.factor(master_data$JobLevel)
sum(is.na(master_data))
work_hrs_percent <- (master_data$avg_wrk_hours/master_data$StandardHours)*100
master_data$wrk_hrs_percent <- work_hrs_percent


#finding leave count
leave_count <- apply(wrk_hours.num, 1, function(x) sum(is.na(x)))
master_data$leave_count <- leave_count

#View(master_data)
el_ot <- as.factor(ifelse(master_data$wrk_hrs_percent < 100,"Early_leavers","Over_time"))
el_ot
master_data$el_ot <- el_ot
#View(master_data)
############################################# Checking for NA in master data and handling the same #################################
sum(is.na(master_data))
colnames(master_data)
sapply(master_data, function(x) sum(is.na(x)))
percentMiss <- function(x){(sum(is.na(x))/length(x))*100}
apply(master_data,2,percentMiss)  
mode(master_data$Education)
str(master_data)
#Checking total NA 
sum(is.na(master_data))

#Checking whether removal of NA will affect the attrition count distribution
master_data[which(is.na(master_data$NumCompaniesWorked) | 
                    is.na(master_data$EnvironmentSatisfaction) | 
                    is.na(master_data$JobSatisfaction) | 
                    is.na(master_data$WorkLifeBalance)),]%>%group_by(Attrition) %>%  summarize(cnt=length(EmployeeID))

master_data%>%group_by(Attrition) %>%  summarize(cnt=length(EmployeeID))
#From above observation,we understand that the removal of NA rows will not affect the attrition in the original dataset ,as the removal counts to only minimal percentage 
#Removing NA 
master_data <- na.omit(master_data)
sum(is.na(master_data))

#First splitting  categorical and numeric variables
num_cols<- Filter(is.numeric, master_data)
colnames(num_cols)
cat_cols <-Filter(is.factor, master_data)
colnames(cat_cols)

################################################### Univariate Analysis ##############
d <- melt(num_cols[,-c(1,4,8)])
#box plot of all numerics columns
plot1 <- ggplot(d, aes(factor(variable), value)) 
plot1 + geom_boxplot() + facet_wrap(~variable, scale="free")
#From above  plots,we observed:
#There exists outliers in columns YearsAtcompany,YearsSincelastpromotion,avg_wrk_hours
#trainingTimesLastYear,StockOptionLevel,TotalWorkingyears



#histogram of all numeric columns
ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()
#From above histogram, we observed that 
#The columns DistanceFromHome,MonthlyIncome,Numcompaniesworked,
#YearsSincelAstPromotion,PercentsalaryHike are all right skewed 

#barplot of all categorical columns

cat_col_names <- names(cat_cols)
df1 <- cat_cols

plot_cat_uni <- function(i)
{
  ggplot(df1, aes(x = df1[,i])) + 
    geom_bar(fill="blue") + labs(title = paste("Bar Chart ",i), x = i)
}

for (i in cat_col_names){
  #print (i)
  readline(prompt="press enter to view plots")
  print(plot_cat_uni(i))
  
  print(df1%>% group_by(df1[,i]) %>% 
          summarise(percent=100*n()/length(df1[,i])) %>% arrange(desc(percent)))
  
}



######################################################### Bivariate Analysis  #########################################



#Getting names of categorical columns
categorical_var <- names(cat_cols)
categorical_var

df <- cat_cols
#Function to generate 100 percent stacked barplot for each columns of dataframe
plot_cat <- function(i)
{
  ggplot(df, aes(x = df[,i], fill = factor(df$Attrition))) + 
    geom_bar(position = "fill") + labs(title = paste("Bar Chart ",i), x = i)
}

for (i in categorical_var){
  
  readline(prompt="press enter to view plots")
  print (i)
  print(plot_cat(i))
  
  print(df%>% group_by(df[,i],by=Attrition) %>% 
          summarise(percent=100*n()/length(df[,i])) %>% arrange(desc(percent)))
  
}



#####OBSERVATIONS FROM THE ABOVE PLOTS

# Attrition rate is 16%
# Max attrition  percent on  category travel_frequently on BusinessTravel
# Max attrition percent occurs in HR department 
# Max attrition percent occurs in HR Education Field
# Max attrition percent occurs in Research Director JobRole 
# Max attrition percent occurs in Single Marital Status
# Max attrition percent occurs in employee group who gave Evironmentsstisfaction score 1 
# Max attrition percent occurs in employee group who gave Jobsatisfaction score 1 
# Max attrition percent occurs in employee group who gave Worklifebalance score 1 
# Max attrition percent occurs in employee group who gave Jobinvolvement score 1 
# Max attrition percent occurs in employee group who got performanceRating as 4

#######################
#DATA PREPARATION 
######################


########################################### outlier detection

#Spliting categrical and numeric
nums <- sapply(master_data, is.numeric)
nums
master_data.num <- master_data[,nums]
str(master_data.num)

cat_cols1 <- !sapply(master_data, is.numeric)
cat_cols1
master_data.cat <- master_data[,cat_cols1]
#View(master_data.num)

#Outlier detection of Continuos variables
quantile(master_data.num$Age,seq(0,1,.01))
quantile(master_data.num$DistanceFromHome,seq(0,1,.01))
quantile(master_data.num$MonthlyIncome,seq(0,1,.01))
master_data.num$MonthlyIncome[master_data.num$MonthlyIncome > 137756.0 ] <- 137756.0
quantile(master_data.num$NumCompaniesWorked,seq(0,1,.01))
quantile(master_data.num$PercentSalaryHike,seq(0,1,.01))
quantile(master_data.num$StockOptionLevel,seq(0,1,.01))
quantile(master_data.num$TotalWorkingYears,seq(0,1,.01))
master_data.num$TotalWorkingYears[master_data.num$TotalWorkingYears > 29] <- 29
quantile(master_data.num$TrainingTimesLastYear,seq(0,1,.01))
quantile(master_data.num$YearsAtCompany,seq(0,1,.01))
master_data.num$YearsAtCompany[which(master_data.num$YearsAtCompany > 17.08)] <- 17.08
quantile(master_data.num$YearsSinceLastPromotion,seq(0,1,.01))
master_data.num$YearsSinceLastPromotion[which(master_data.num$YearsSinceLastPromotion > 9)] <- 9
quantile(master_data.num$YearsWithCurrManager,seq(0,1,.01))
master_data.num$YearsWithCurrManager[which(master_data.num$YearsWithCurrManager > 14)] <- 14
quantile(master_data.num$wrk_hrs_percent,seq(0,1,.01))
master_data.num$wrk_hrs_percent[which(master_data.num$wrk_hrs_percent > 133.72634)] <- 133.72634
quantile(master_data.num$avg_wrk_hours,seq(0,1,.01))
quantile(master_data.num$leave_count,seq(0,1,.01))


# confirming the outlier removal, and ignoring minimal presense of outliers
d <- melt(master_data.num)
#box plot of all numerics columns
plot1 <- ggplot(d, aes(factor(variable), value)) 
plot1 + geom_boxplot() + facet_wrap(~variable, scale="free")


#Feature Standardisation
#Scaling numeric vaues other than Employee_id, standard hours and Employee count 
master_data.num[,-c(1,4,8)] <- apply(master_data.num[,-c(1,4,8)],2,scale)
#View(master_data.num)
#View(master_data.cat)


################################################## Creating Dummies for factor variables ########################

#Columns with less than 3 levels 
master_data_subset <- master_data.cat[,c("Attrition","Gender","Over18","PerformanceRating","el_ot")]
levels(master_data_subset$Attrition)
#[1] "No"  "Yes"
levels(master_data_subset$Attrition) <- c(0,1)
levels(master_data_subset$Gender)
#[1] "Female" "Male"  
levels(master_data_subset$Gender) <- c(0,1)
levels(master_data_subset$Over18) <- c(1)
levels(master_data_subset$PerformanceRating)
levels(master_data_subset$PerformanceRating) <- c(0,1)
levels(master_data_subset$el_ot)
#[1] "Early_leavers" "Over_time" 
levels(master_data_subset$el_ot) <- c(0,1)
str(master_data_subset)

#Creating Dummies for factor variables with level more than 2.
str(master_data.cat)
dummy_BT <- data.frame(model.matrix(~BusinessTravel-1,data=master_data.cat)[,-1])
str(dummy_BT)
dummy_jl <- data.frame(model.matrix(~JobLevel-1,data=master_data.cat)[,-1])
str(dummy_jl)
dummy_Dep <- data.frame(model.matrix(~Department-1,data=master_data.cat)[,-1])
str(dummy_Dep)
dummy_edu <- data.frame(model.matrix(~Education-1,data=master_data.cat)[,-1])
str(dummy_edu)
dummy_edufield <- data.frame(model.matrix(~EducationField-1,data=master_data.cat)[,-1])
str(dummy_edufield)
dummy_jobrole <- data.frame(model.matrix(~JobRole-1,data=master_data.cat)[,-1])
str(dummy_jobrole)
dummy_ms <- data.frame(model.matrix(~MaritalStatus-1,data=master_data.cat)[,-1])
str(dummy_ms)
dummy_es <- data.frame(model.matrix(~EnvironmentSatisfaction-1,data=master_data.cat)[,-1])
str(dummy_es)
dummy_js <- data.frame(model.matrix(~JobSatisfaction-1,data=master_data.cat)[,-1])
str(dummy_js)
dummy_wlb <- data.frame(model.matrix(~WorkLifeBalance-1,data=master_data.cat)[,-1])
str(dummy_wlb)
dummy_jin <- data.frame(model.matrix(~JobInvolvement-1,data=master_data.cat)[,-1])
str(dummy_jin)

#Binding all Variables
master_final <- cbind(master_data.num,master_data_subset,dummy_jl,dummy_BT,dummy_Dep,dummy_edu,dummy_edufield,dummy_es,dummy_jin,dummy_jobrole,dummy_js)
#View(master_final)

#Converting factor values to numeric
str(master_final)
master_final$Attrition <- as.numeric(as.character(master_final$Attrition))
master_final$Gender <- as.numeric(as.character(master_final$Gender))
master_final$Over18 <- as.numeric(as.character(master_final$Over18))
master_final$PerformanceRating <- as.numeric(as.character(master_final$PerformanceRating))
master_final$el_ot <- as.numeric(as.character(master_final$el_ot))
str(master_final)

#******************************************************************************************************************************************************
########################################################################## STEP 4: MODEL BUILDING  #######################################################
#******************************************************************************************************************************************************
#splitting of train and test dataset
set.seed(100)
train_indices <- sample.split(master_final$Attrition,SplitRatio=0.7)
train <- master_final[train_indices,]
#View(train)
test <- master_final[!(train_indices),]
#View(test)
colnames(train)
View(train)

#MODEL:model_1
#Removed: Nothing 
model_1 <- glm(Attrition~.,data=train,family="binomial")
summary(model_1)


#MODEL:model_2
#Removed: By StepAIC
model_2 <- stepAIC(model_1,direction="both")
summary(model_2)
sort(vif(model_2))



#MODEL:model_3
#Excluding:  EducationFieldLife.Sciences 
model_3 <-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                YearsWithCurrManager + PerformanceRating + el_ot + JobLevel2 + 
                BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                Education5 +EducationFieldMarketing + 
                EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobInvolvement2 + JobInvolvement3 + JobRoleManufacturing.Director + 
                JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                JobSatisfaction3 + JobSatisfaction4 + JobRoleManager, family = "binomial", 
              data = train)
summary(model_3)
sort(vif(model_3))


#MODEL:model_4
#Excluding: YearsAtCompany +
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear +  YearsSinceLastPromotion + 
                 YearsWithCurrManager + PerformanceRating + el_ot + JobLevel2 + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 Education5 +EducationFieldMarketing + 
                 EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobInvolvement2 + JobInvolvement3 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + JobRoleManager, family = "binomial", 
               data = train)
summary(model_4)
sort(vif(model_4))



#MODEL:model_4a
#Excluding: BusinessTravelTravel_Rarely +
model_4a <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear +  YearsSinceLastPromotion + 
                  YearsWithCurrManager + PerformanceRating + el_ot + JobLevel2 + 
                  BusinessTravelTravel_Frequently +  
                  Education5 +EducationFieldMarketing + 
                  EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobInvolvement2 + JobInvolvement3 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + JobRoleManager, family = "binomial", 
                data = train)
summary(model_4a)
sort(vif(model_4a))

#MODEL:model_5
#Excluding:  EducationFieldMedical + 
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear +  YearsSinceLastPromotion + 
                 YearsWithCurrManager + PerformanceRating + el_ot + JobLevel2 + 
                 BusinessTravelTravel_Frequently +  
                 Education5 +EducationFieldMarketing + 
                 EducationFieldOther + EducationFieldTechnical.Degree + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobInvolvement2 + JobInvolvement3 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + JobRoleManager, family = "binomial", 
               data = train)
summary(model_5)
sort(vif(model_5))

#MODEL:model_6
#Excluding:  EducationFieldMarketing +
model_6 <-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear +  YearsSinceLastPromotion + 
                YearsWithCurrManager + PerformanceRating + el_ot + JobLevel2 + 
                BusinessTravelTravel_Frequently +  
                Education5 + 
                EducationFieldOther + EducationFieldTechnical.Degree + 
                EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobInvolvement2 + JobInvolvement3 + JobRoleManufacturing.Director + 
                JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                JobSatisfaction3 + JobSatisfaction4 + JobRoleManager, family = "binomial", 
              data = train)
summary(model_6)
sort(vif(model_6))


#MODEL:model_7
#Excluding:  EducationFieldTechnical.Degree + 
model_7 <-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear +  YearsSinceLastPromotion + 
                YearsWithCurrManager + PerformanceRating + el_ot + JobLevel2 + 
                BusinessTravelTravel_Frequently +  
                Education5 + 
                EducationFieldOther + 
                EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobInvolvement2 + JobInvolvement3 + JobRoleManufacturing.Director + 
                JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                JobSatisfaction3 + JobSatisfaction4 + JobRoleManager, family = "binomial", 
              data = train)
summary(model_7)
sort(vif(model_7))

#MODEL:model_8
#Excluding:+ JobRoleManager
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear +  YearsSinceLastPromotion + 
                 YearsWithCurrManager + PerformanceRating + el_ot + JobLevel2 + 
                 BusinessTravelTravel_Frequently +  
                 Education5 + 
                 EducationFieldOther + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobInvolvement2 + JobInvolvement3 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 , family = "binomial", 
               data = train)
summary(model_8)
sort(vif(model_8))



#MODEL:model_9
#Excluding:   EducationFieldOther + 
model_9 <-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear +  YearsSinceLastPromotion + 
                YearsWithCurrManager + PerformanceRating + el_ot + JobLevel2 + 
                BusinessTravelTravel_Frequently + Education5 + 
                EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobInvolvement2 + JobInvolvement3 + JobRoleManufacturing.Director + 
                JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                JobSatisfaction3 + JobSatisfaction4 , family = "binomial", 
              data = train)
summary(model_9)
sort(vif(model_9))

#MODEL:model_10
#Excluding: Education5 + 
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear +  YearsSinceLastPromotion + 
                  YearsWithCurrManager + PerformanceRating + el_ot + JobLevel2 + 
                  BusinessTravelTravel_Frequently + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobInvolvement2 + JobInvolvement3 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 , family = "binomial", 
                data = train)
summary(model_10)
sort(vif(model_10))

#MODEL:model_11
#Excluding:  PerformanceRating +
model_11<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear +  YearsSinceLastPromotion + 
                 YearsWithCurrManager +  el_ot + JobLevel2 + 
                 BusinessTravelTravel_Frequently + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobInvolvement2 + JobInvolvement3 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 , family = "binomial", 
               data = train)
summary(model_11)
sort(vif(model_11))


#MODEL:model_12
#Excluding: JobInvolvement2 + 
model_12<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear +  YearsSinceLastPromotion + 
                 YearsWithCurrManager +  el_ot + JobLevel2 + 
                 BusinessTravelTravel_Frequently + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobInvolvement3 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 , family = "binomial", 
               data = train)
summary(model_12)
sort(vif(model_12))


#MODEL:model_13
#Excluding:  JobInvolvement3
model_13<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear +  YearsSinceLastPromotion + 
                  YearsWithCurrManager +  el_ot + JobLevel2 + 
                  BusinessTravelTravel_Frequently + 
                  EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 , family = "binomial", 
                data = train)
summary(model_13)
sort(vif(model_13))

#MODEL:model_14
#Excluding:   JobLevel2 + 
model_14<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear +  YearsSinceLastPromotion + 
                 YearsWithCurrManager +  el_ot + 
                 BusinessTravelTravel_Frequently + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 , family = "binomial", 
               data = train)
summary(model_14)
sort(vif(model_14))


#MODEL:model_15
#Excluding:  JobRoleSales.Executive + 
model_15<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear +  YearsSinceLastPromotion + 
                 YearsWithCurrManager +  el_ot + 
                 BusinessTravelTravel_Frequently + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 , family = "binomial", 
               data = train)
summary(model_15)
sort(vif(model_15))


#MODEL:model_16
#Excluding:  JobRoleResearch.Director
model_16<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear +  YearsSinceLastPromotion + 
                 YearsWithCurrManager +  el_ot + 
                 BusinessTravelTravel_Frequently + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobRoleManufacturing.Director + 
                 JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 , family = "binomial", 
               data = train)
summary(model_16)
sort(vif(model_16))


#MODEL:model_17
#Excluding:  TrainingTimesLastYear + 
model_17<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + 
                 YearsWithCurrManager +  el_ot + 
                 BusinessTravelTravel_Frequently + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobRoleManufacturing.Director + 
                 JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 , family = "binomial", 
               data = train)
summary(model_17)
sort(vif(model_17))

#MODEL:model_18
#Excluding:  JobSatisfaction2 + 
model_18<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager +  el_ot + 
                 BusinessTravelTravel_Frequently + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobRoleManufacturing.Director + JobSatisfaction3 + 
                 JobSatisfaction4 , family = "binomial", data = train)
summary(model_18)
sort(vif(model_18))


#MODEL:model_19
#Excluding:  JobSatisfaction3 + 
model_19<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager +  el_ot + 
                 BusinessTravelTravel_Frequently + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobRoleManufacturing.Director + 
                 JobSatisfaction4 , family = "binomial", data = train)
summary(model_19)
sort(vif(model_19))


final_model <- model_19


#******************************************************************************************************************************************************
########################################################################## STEP 5: MODEL EVALUATION  #######################################################
#******************************************************************************************************************************************************
View(train)
test_pred <- predict(final_model,type="response",newdata =test[,-18])
summary(test_pred)
test$prob <- test_pred

#######View(test)

test_pred_left <- factor(ifelse(test_pred>=0.50,"Yes","No"))
test_act_left <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_pred_left,test_act_left)
library(rminer)
mmetric(test_act_left,test_pred_left,c("ACC"))

library(caret)
test_conf <- confusionMatrix(test_pred_left, test_act_left, positive = "Yes")
test_conf


# Let's Choose the cutoff value. 
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_left <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_left, test_act_left, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Summary of test probability

summary(test_pred)

s = seq(0.001169,0.853788  ,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,0.01),cex.lab=1.5)
axis(2,seq(0,1,0.01),cex.lab=1.5)
#axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
#axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
#optimal cutoff is 
cutoff



test_cutoff_left <- factor(ifelse(test_pred >=0.1734153, "Yes", "No"))

#
conf_final <-confusionMatrix(test_cutoff_left, test_act_left, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_left <- ifelse(test_cutoff_left=="Yes",1,0)
test_act_left <- ifelse(test_act_left=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_left, test_act_left)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

left_decile = lift(test_act_left, test_pred, groups = 10)
left_decile
write.csv(left_decile,file="temp4.csv")

###############################################   END ###################################