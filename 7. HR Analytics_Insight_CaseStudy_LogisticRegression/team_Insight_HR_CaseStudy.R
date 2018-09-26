
## Creating and Setting working directory 
dir.create("C:/HR_CaseStudy")
setwd("C:/HR_CaseStudy")

## Downloading assignment files
filepath1 <- "https://cdn.upgrad.com/UpGrad/temp/c913c97e-059f-418e-b1d5-f7d9151c2dbd/PA-I_Case_Study_HR_Analytics.zip"
#downloading the zip file
download.file(filepath1, destfile = "PA-I_Case_Study_HR_Analytics.zip")

zipF<- "C:/HR_CaseStudy/PA-I_Case_Study_HR_Analytics.zip"
outDir<-"C:/HR_CaseStudy"
##Extracting the files from zip file 
unzip(zipF,exdir=outDir)

################ RequiredPackages
requiredPackages = c('MASS','car','caret','cowplot','ggplot2','GGally','caTools','lubridate','e1071')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)){ 
    install.packages(p)
  }
  library(p,character.only = TRUE)
}


#Loading the 5 files
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
general_data <- read.csv("general_data.csv", stringsAsFactors = FALSE)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)
intime_data <- read.csv("in_time.csv", stringsAsFactors = FALSE)
outtime_data <- read.csv("out_time.csv", stringsAsFactors = FALSE)


#######################################################################################################
#### ---------------------------------- Data Preparation ------------------------------------ #########

##Calculating in_time and out_time of employees over 12 months period and storing it in "office_time"
#Remove the holiday columns from "in_time" and "out_time" dataset (Cleaning Data)
remove_holiday_col <- function(x){
  time_data <- subset(x, select = -c(X2015.01.01,X2015.01.14,X2015.01.26,
                                     X2015.03.05,X2015.05.01,X2015.12.25,
                                     X2015.11.09,X2015.11.10,X2015.11.11,
                                     X2015.10.02,X2015.09.17,X2015.07.17))
  return(time_data)
}

intime_data <- remove_holiday_col(intime_data)
outtime_data <- remove_holiday_col(outtime_data)

sum(is.na(intime_data))
sum(is.na(outtime_data))

str(intime_data)
str(outtime_data)

#Convert all the date columns from character to DateTime format
intime_data[,2:250] <- lapply(intime_data[,2:250], function(x) as_datetime(x))
outtime_data[,2:250] <- lapply(outtime_data[,2:250], function(x) as_datetime(x))

#We Calculate the work hours of each employee everyday outtime-intime
Work_hrs <- outtime_data[,2:250]-intime_data[,2:250]
Work_hrs_y <- cbind(intime_data[,c("X")],Work_hrs)
names(Work_hrs_y)[1]<-paste("X")
Work_hrs_y[,2:250] <- lapply(Work_hrs_y[,2:250], function(x) as.numeric(x))

#Calculaing the avg work hours of each employee
Work_hrs_y$work_avg <- rowMeans(Work_hrs_y[,2:250], na.rm = TRUE)


######

#Structure of the data sets
    str(employee_survey_data)
    str(general_data)
    str(manager_survey_data)

#Create a new dataframe consisting of Employee ID and Office time hrs.
    office_time <- data.frame(Work_hrs_y$X, Work_hrs_y$work_avg)
    colnames(office_time) <- c("EmployeeID", "OfficeHrs")
    office_time$OfficeHrs <- round(office_time$OfficeHrs, 2)

#Collating the data together
    length(unique(tolower(employee_survey_data$EmployeeID)))  # 4410, Employee ID is the primary key
    length(unique(tolower(general_data$EmployeeID)))          # 4410, Employee ID is the primary key
    length(unique(tolower(manager_survey_data$EmployeeID)))   # 4410, Employee ID is the primary key
    length(unique(tolower(office_time$EmployeeID)))           # 4410, Employee ID is the primary key


    setdiff(employee_survey_data$EmployeeID,general_data$EmployeeID)
    setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID)
    setdiff(employee_survey_data$EmployeeID,office_time$EmployeeID)

# Merge all the files to create a collective data set
    hr_analytics <- merge(employee_survey_data,general_data, by="EmployeeID", all = F)
    hr_analytics <- merge(hr_analytics, manager_survey_data, by="EmployeeID", all = F)
    hr_analytics <- merge(hr_analytics, office_time, by= "EmployeeID", all = F)

#Create a new derived column "Over_time"
hr_analytics$Over_time <- ifelse(hr_analytics$OfficeHrs > 8, 1, 0)

#Create inadequate work time
hr_analytics$inadq_time <- ifelse(hr_analytics$OfficeHrs < 7, 1, 0)

### create no of leaves as derived metric
### it is calculated by counting the no of NA's in (out_time-in_time) which we have stored as Work_hrs
for(i in 1:4410) {
  
  hr_analytics$no_leaves[i]<-sum(is.na(Work_hrs[i,]))
}

View(hr_analytics)

#######################################################################################################
######### ------------------------------------- EDA  --------------------------------------- ##########

#Distribution of categorical variables
par(mfrow = c(4,2)) ## par can be used to set or query graphical parameters.
## creating a graphical matrix of 4*2 matrix format 
par(mar =rep(2,4)) ## it gives the number of lines of margin to be specified on the four sides of the plot

      barplot(table(hr_analytics$Attrition), main = "Attrition Distribution")
      barplot(table(hr_analytics$JobSatisfaction), main="Job Satisfication level")
      barplot(table(hr_analytics$BusinessTravel), main = "Business Travel")
      barplot(table(hr_analytics$Gender), main ="Gender")
      barplot(table(hr_analytics$MaritalStatus), main = "Marital Status")
      barplot(table(hr_analytics$JobInvolvement), main = "Job Involvement")
      barplot(table(hr_analytics$Over_time), main = "Over time")
      barplot(table(hr_analytics$PerformanceRating), main = "Performance rating")


str(hr_analytics)

#cleaning the individual NA values from the data set
xyz<-c(2,3,4,13,18) 
##### cleaning NA for Environment Satisfaction(replace with median,since if we remove this NA it will cause problem with our analysis)
hr_analytics$EnvironmentSatisfaction[which(is.na(hr_analytics$EnvironmentSatisfaction))]<-median(hr_analytics$EnvironmentSatisfaction,na.rm = TRUE)

##### cleaning NA for Job Satisfaction(replace with median,since if we remove this NA it will cause problem with our analysis)
hr_analytics$JobSatisfaction[which(is.na(hr_analytics$JobSatisfaction))]<-median(hr_analytics$JobSatisfaction,na.rm = TRUE)

##### cleaning NA for Job Satisfaction(replace with median,since if we remove this NA it will cause problem with our analysis)
hr_analytics$WorkLifeBalance[which(is.na(hr_analytics$WorkLifeBalance))]<-median(hr_analytics$WorkLifeBalance,na.rm = TRUE)

##### cleaning NA for NumCompaniesWorked(replace with mean,since if we remove this NA it will cause problem with our analysis)
hr_analytics$NumCompaniesWorked[which(is.na(hr_analytics$NumCompaniesWorked))]<-mean(hr_analytics$NumCompaniesWorked,na.rm = TRUE)

##### cleaning NA for TotalWorkingYears(replace with mean,since if we remove this NA it will cause problem with our analysis)
hr_analytics$TotalWorkingYears[which(is.na(hr_analytics$TotalWorkingYears))]<-mean(hr_analytics$TotalWorkingYears,na.rm = TRUE)


##list of categorical attributes :-
#      1.)With  2 levels:- 
#          Attrition, Gender, Over18  
#      
#       2.)More than 2 levels :_
#        EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,
#        JobRole, MaritalStatus,  BusinessTravel, Department,Education, EducationField
#        JobInvolvement,JobLevel,PerformanceRating


### First Convert all binary categorical variables
##### Converting the "Attrition,Gender and Over18" attributes with 2 levels into numbers(0,1)
hr_analytics$Attrition <- ifelse(hr_analytics$Attrition == "Yes", 1,0)
hr_analytics$Gender <- ifelse(hr_analytics$Gender == "Female",1,0)
hr_analytics$Over18 <- ifelse(hr_analytics$Over18 == "Y", 1,0)


##### Create a dataframe of categorical varibables with more than 2 levels
hr_analytics_fact <- hr_analytics[,c("EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance",
                                     "BusinessTravel","Department","EducationField", "Education",
                                     "JobRole","MaritalStatus","JobInvolvement","JobLevel",
                                     "PerformanceRating")]

##################################################################################################
##################### Convert Multi Categorical variables to dummy variables #####################
hr_analytics_fact <- data.frame(sapply(hr_analytics_fact, function(x) factor(x)))
str(hr_analytics_fact)

#####Creating dummy attributes for factor attributes
dummies <- data.frame(sapply(hr_analytics_fact, function(x)
  data.frame(model.matrix(~x-1, data = hr_analytics_fact))[,-1]))

####Removing the categorical attributes and adding the corresponding dummy attributes.
hr_analytics <- cbind(hr_analytics[,-c(2,3,4,7,8,10,11,14,15,16,28,29)], dummies)
View(hr_analytics)  # 4410 observations with 44 attributes, master_file2

str(hr_analytics)

###Ploting graph for better understanding
par(mfrow = c(4,2))
par(mar = rep(2,4))
    hist(hr_analytics$Age, main = "Age")
    hist(hr_analytics$DistanceFromHome, main = "Distance From Home")
    hist(hr_analytics$MonthlyIncome, main = "Monthly Income")
    hist(hr_analytics$NumCompaniesWorked, main = "Num Companies Worked")
    hist(hr_analytics$PercentSalaryHike, main = "Percent Salary Hike")
    hist(hr_analytics$TrainingTimesLastYear, main = "Training Times LastYear")
    hist(hr_analytics$YearsAtCompany, main = "Years At Company")
    hist(hr_analytics$YearsSinceLastPromotion, main = "Years Since Last Promotion")

###Ploting graph for better understanding 
par(mfrow = c(4,2))
par(mar = rep(2,4))
      boxplot(hr_analytics$Age, main = "Age")
      boxplot(hr_analytics$DistanceFromHome, main = "Distance From Home" )
      boxplot(hr_analytics$MonthlyIncome, main = "Monthly Income")
      boxplot(hr_analytics$NumCompaniesWorked, main = "Num Companies Worked")
      boxplot(hr_analytics$PercentSalaryHike, main = "Percent Salary Hike")
      boxplot(hr_analytics$TrainingTimesLastYear, main = "Training Times LastYear")
      boxplot(hr_analytics$YearsAtCompany, main = "Years At Company")
      boxplot(hr_analytics$YearsSinceLastPromotion, main = "Years Since Last Promotion")
### with the help of box plot we were able to see the outliers in the data 
### but it is not good to remove them as they represent the 
### company population where such data is bound to exist

summary(hr_analytics)

hr_data_cor <- select(hr_analytics, Age, DistanceFromHome, Education, JobLevel, MonthlyIncome,
                      NumCompaniesWorked, PercentSalaryHike, StockOptionLevel, TotalWorkingYears, TrainingTimesLastYear,
                      YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager, Avg_Work_Hour)
Corl <- cor(hr_data_cor)
corrplot(Corl, type = "lower", method = "number", number.cex = .7, tl.srt = 45)


#scaling the variables
#indicies of all the columns to be scaled
ind<-c(2,4,7,8,10,c(12:18),21)
hr_analytics_scaled<-hr_analytics
for(i in ind)
{
  hr_analytics_scaled[,i]<-scale(x=hr_analytics[,i],center = TRUE,scale = TRUE)
}
summary(hr_analytics_scaled)

# Check the range of the scaled data
sapply(hr_analytics_scaled, function(x) range(x))


#################################################################################
############################### Modelling #######################################
# First divide the data in to training and test data
set.seed(100)

indices = sample.split(hr_analytics$EmployeeID, SplitRatio = 0.7)

train = hr_analytics_scaled[indices,c(3,2,c(4:61))]

test = hr_analytics_scaled[!(indices),c(3,2,c(4:61))]


# Now let us start building the model.

model_1<-glm(Attrition~.,data=train,family = 'binomial')
summary(model_1)

### For quicker variable reduction,we use stepAIC, which reduces variables 
### based on Akaike information criterion
model_2<-stepAIC(model_1, direction="both")
summary(model_2)

vif(model_2)

##### removing EducationField.xLife.Sciences(as it is less significant for our model)

model_3<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5 + PerformanceRating, 
             family = "binomial", data = train)
summary(model_3)
vif(model_3)

##### removing YearsAtCompany (High vif and less significant for our model)

model_4<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5 + PerformanceRating, 
             family = "binomial", data = train)
summary(model_4)
vif(model_4)

###### removing MaritalStatus.xMarried (as it is less significant for our model)

model_5<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5 + PerformanceRating, 
             family = "binomial", data = train)
summary(model_5)
vif(model_5)
 
###### removing PerformanceRating (as it is less significant for our model)

model_6<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5, 
             family = "binomial", data = train)
summary(model_6)
vif(model_6)

###### removing PercentSalaryHike (as it is less significant for our model)

model_7<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5, 
             family = "binomial", data = train)
summary(model_7)
vif(model_7)

###### removing JobRole.xSales.Executive (as it is less significant for our model)

model_8<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director +   
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5, 
             family = "binomial", data = train)
summary(model_8)
vif(model_8)

###### removing JobInvolvement.x2 (as it is less significant for our model)

model_9<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle + JobInvolvement.x3 + 
               JobLevel.x5, 
             family = "binomial", data = train)
summary(model_9)
vif(model_9)

###### removing JobLevel.x5 (as it is less significant for our model)

model_10<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle + JobInvolvement.x3, 
             family = "binomial", data = train)
summary(model_10)
vif(model_10)

###### removing Education.x2 (as it is less significant for our model)

model_11<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xSingle + JobInvolvement.x3, 
              family = "binomial", data = train)
summary(model_11)
vif(model_11)

###### removing JobRole.xResearch.Director (as it is less significant for our model)

model_12<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + JobInvolvement.x3, 
              family = "binomial", data = train)
summary(model_12)
vif(model_12)

###### removing Department.xResearch...Development (High Vif and less significant for our model)

model_13<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely +  
                Department.xSales + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + JobInvolvement.x3, 
              family = "binomial", data = train)
summary(model_13)
vif(model_13)

###### removing Department.xSales (as it is less significant for our model)

model_14<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely +  
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + JobInvolvement.x3, 
              family = "binomial", data = train)
summary(model_14)
vif(model_14)

###### removing JobInvolvement.x3 (as it is less significant for our model)

model_15<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + JobRole.xManufacturing.Director + MaritalStatus.xSingle, 
              family = "binomial", data = train)
summary(model_15)
vif(model_15)

###### removing BusinessTravel.xTravel_Rarely (High Vif and is less significant for our model)

model_16<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, 
              family = "binomial", data = train)
summary(model_16)
vif(model_16)

###### removing TrainingTimesLastYear (as it is less significant for our model)

model_17<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, 
              family = "binomial", data = train)
summary(model_17)
vif(model_17)

###### removing Age (as it is less significant for our model)

model_18<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, 
              family = "binomial", data = train)
summary(model_18)
vif(model_18)

final_model<-model_18
#######################################################################

### Model Evaluation

### Test Data ####

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

summary(test_pred)

test$prob <- test_pred
View(test)

test_actual_attrition<-factor(ifelse(test$Attrition==1,"Yes","No"))


###### Cutoff>=0.5
test_predict_attrition<-factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_conf <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_conf

##### Cutoff>=0.4
test_predict_attrition<-factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_conf <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_conf

###### Finding the Optimal Probability Cutoff

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
dev.off()

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,1.0,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


op_cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]

#####Optimal Cutoff  =  0.1536364
test_predict_attrition<-factor(ifelse(test_pred >= op_cutoff, "Yes", "No"))
conf_final <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
conf_final

#######   Accuracy    : 0.7513 
#######   Sensitivity : 0.7642          
#######   Specificity : 0.7486 



#Creating dataframe for calc LIFT & GAIN Test and KSS test
lift_gain<-test[,c(1,61)]
nrow(lift_gain)
#Sorting the df according to Predicted P's
lift_gain<-lift_gain[order(lift_gain$prob,decreasing = TRUE),]

total<-sum(lift_gain$Attrition)
total2<-1323-total 

#Initializing Lift & Gain and KSS Variables
decile<-c(1:10)
Obs<-c(1,133,266,399,531,663,795,927,1059,1191)
Obs2<-c(133,266,399,531,663,795,927,1059,1191,1323)
obs3<-c(133,133,133,132,132,132,132,132,132,132)

Churn<-vector(mode = 'numeric',length = 10)
Cum_Churn<-vector(mode = 'numeric',length = 10)
Gain<-vector(mode = 'numeric',length = 10)
Lift<-vector(mode = 'numeric',length = 10)
Non_Churn<-vector(mode = 'numeric',length = 10)
Cum_Non_Churn<-vector(mode = 'numeric',length = 10)
Gain_rand<-c(1:10)/10

#temporary  variables
t<-0
t2<-0

####Calculating Lift & Gain and KSS
for(i in 1:10) {
  Churn[i]<-sum(lift_gain$Attrition[Obs[i]:Obs2[i]])
  t<-t+Churn[i]
  Cum_Churn[i]<-t
  
  Non_Churn[i]<-obs3[i]-Churn[i]
  t2<-t2+Non_Churn[i]
  Cum_Non_Churn[i]<-t2
  
  Gain[i]<-Cum_Churn[i]/total
  Lift[i]<-Gain[i]/Gain_rand[i]
}
KSS<-Gain-Cum_Non_Churn/total2

Churn
Non_Churn
Cum_Churn
Cum_Non_Churn
Gain
Lift
KSS
plot(x=Lift,y=decile,type = 'o',col = "red", main = "Lift Chart")
plot(x=Gain,y=decile,type ='o', col = "blue", main = "Gain Chart")
plot(x=decile,y=KSS,type = 'o', col = "green", main = "KSS Chart")


