
# intall the required packages here 

library(tidyr) 
library(dplyr)
library(ggplot2)  
library(Hmisc) 
library(stringr) 
library(corrplot) 
library(MASS)
library(car)

# Set working directory and load the file.

setwd("C:/Users/ADMIN/Desktop/Linear Regression/Linear Regression - Assignment")
CarPrice <- read.csv("CarPrice_Assignment.csv", header = TRUE, stringsAsFactors = FALSE)
str(CarPrice)
dim(CarPrice)


#######################################################################################################
######### --------------------------------- Data Cleaning  ---------------------------------- #########

# Check for duplicate values:
unique(CarPrice)
sum(duplicated(CarPrice))
# No duplicate variable is present

# Check for missing values:
sum(is.na(CarPrice)) 
# No NA values are present

#######################################################################################################
#### ---------------------------------- Data Preparation ------------------------------------ #########
#1. As per instructions in the assignment, separate Car company name and Car model name 
#2. Remove the discrepancies in the Car company name 

# Separate CarName into car model name and Car company name
CarPrice <- separate(CarPrice, CarName, into = c("Car_Company", "Car_Model"), sep = " ")
# We need to consider only company name as the independent variable for the model building. 
# So we can discard Car_Model variable
CarPrice$Car_Model <- NULL

# Correcting the case and misspelt Car Company name
CarPrice$Car_Company[which(CarPrice$Car_Company == "maxda")] <- "mazda"
CarPrice$Car_Company[which(CarPrice$Car_Company == "porcshce")] <- "porsche"
CarPrice$Car_Company[which(CarPrice$Car_Company == "toyouta")] <- "toyota"
CarPrice$Car_Company[which(CarPrice$Car_Company == "vokswagen")] <- "volkswagen"
CarPrice$Car_Company[which(CarPrice$Car_Company == "vw")] <- "volkswagen"

CarPrice$Car_Company <- capitalize(CarPrice$Car_Company)

#######################################################################################################
######### ------------------------------------- EDA  --------------------------------------- ##########

# Check for outliers and perform capping if required

#1. Weelbase of car (Numeric)
boxplot(CarPrice$wheelbase, main = "Car_Wheelbase") # Outliers Present
quantile(CarPrice$wheelbase, seq(0,1,0.01))
CarPrice$wheelbase[which(CarPrice$wheelbase > 114.200)] <- 114.200
boxplot(CarPrice$wheelbase, main = "Car_Wheelbase")


#2. Length of car (Numeric)
boxplot(CarPrice$carlength, main = "Car_Length") # Oulier Present
quantile(CarPrice$carlength, seq(0,1,0.01))
CarPrice$carlength[which(CarPrice$carlength < 144.816)] <- 144.816
boxplot(CarPrice$carlength, main = "Car_Length")

#3. Width of car (Numeric)
boxplot(CarPrice$carwidth, main = "Car_Width") # Oulier Present
quantile(CarPrice$carwidth, seq(0,1,0.01))
CarPrice$carwidth[which(CarPrice$carwidth > 70.460)] <- 70.460
boxplot(CarPrice$carwidth, main = "Car_Width")


#4. height of car (Numeric)
boxplot(CarPrice$carheight, main = "Car_Height") # No Oulier Present
quantile(CarPrice$carheight, seq(0,1,0.01))


#5. The weight of a car without occupants or baggage. (Numeric)
boxplot(CarPrice$curbweight, main = "Car_Weight") # No Oulier Present
quantile(CarPrice$curbweight, seq(0,1,0.01))


#6. Size of car (Numeric)	
boxplot(CarPrice$enginesize, main = "Car_Engine_Size") # Oulier Present
quantile(CarPrice$enginesize, seq(0,1,0.01))
CarPrice$enginesize[which(CarPrice$enginesize > 183.00)] <- 183.00
boxplot(CarPrice$enginesize, main = "Car_Engine_Size")


#7. Boreratio of car (Numeric)
boxplot(CarPrice$boreratio, main = "Car Boreratio") # No Oulier Present
quantile(CarPrice$boreratio, seq(0,1,0.01))


#8. Stroke or volume inside the engine (Numeric)
boxplot(CarPrice$stroke, main = "Car_Stroke") # Oulier Present both sides
quantile(CarPrice$stroke, seq(0,1,0.01))
CarPrice$stroke[which(CarPrice$stroke < 2.6512)] <- 2.6512
CarPrice$stroke[which(CarPrice$stroke > 3.8600)] <- 3.8600
boxplot(CarPrice$stroke, main = "Car_Stroke")

#9. compression ratio of car (Numeric) 
boxplot(CarPrice$compressionratio, main = "Car_CompressionRatio")  # Oulier Present
quantile(CarPrice$compressionratio, seq(0,1,0.01))
CarPrice$compressionratio[which(CarPrice$compressionratio > 10.0000)] <- 10.0000
boxplot(CarPrice$compressionratio, main = "Car_CompressionRatio") 


#10. Horsepower (Numeric)	
boxplot(CarPrice$horsepower, main = "Car_Horsepower")  # Oulier Present
quantile(CarPrice$horsepower, seq(0,1,0.01))
CarPrice$horsepower[which(CarPrice$horsepower > 184.00)] <- 184.00
boxplot(CarPrice$horsepower, main = "Car_Horsepower")


#11. Car peak rpm (Numeric)	
boxplot(CarPrice$peakrpm, main = "Car_PeakRPM") # Oulier Present
quantile(CarPrice$peakrpm, seq(0,1,0.01))
CarPrice$peakrpm[which(CarPrice$peakrpm > 5980)] <- 5980
boxplot(CarPrice$peakrpm, main = "Car_PeakRPM")


#12. Mileage in city (Numeric)
boxplot(CarPrice$citympg, main = "Car_CityMPG") # Oulier Present
quantile(CarPrice$citympg, seq(0,1,0.01))
CarPrice$citympg[which(CarPrice$citympg > 38.00)] <- 38.00
boxplot(CarPrice$citympg, main = "Car_CityMPG")


#13. Mileage on highway (Numeric)	
boxplot(CarPrice$highwaympg, main = "City_HighWayMPG") # Oulier Present
quantile(CarPrice$highwaympg, seq(0,1,0.01))
CarPrice$highwaympg[which(CarPrice$highwaympg > 46.92)] <- 46.92
boxplot(CarPrice$highwaympg, main = "City_HighWayMPG")


#14. Price of car (Numeric)
boxplot(CarPrice$price, main = "Price") # Oulier Present
quantile(CarPrice$price, seq(0,1,0.01))


# Bivariate/ Multivariate Analysis - Corellation Plot
boxplot(CarPrice$highwaympg ~ CarPrice$cylindernumber)
ggplot(CarPrice, aes(x=CarPrice$cylindernumber, y= CarPrice$price)) + geom_point()
ggplot(CarPrice, aes(x= CarPrice$horsepower, y= CarPrice$price)) + geom_line()


######### ----------------------------------  Derived Metrices -------------------------------------- ##########

#1. Power to Weight Ratio
CarPrice$PWR <- round(CarPrice$horsepower/CarPrice$curbweight, digits = 3)
plot(CarPrice$price, CarPrice$PWR)

#2. Torque = (HP*5252)/RPM
CarPrice$Torque <- (CarPrice$horsepower*5252)/CarPrice$peakrpm
plot(CarPrice$price, CarPrice$Torque)

# Check the correlation between numeric variables
CarPrice_Numeric <- select(CarPrice, wheelbase, carlength, carwidth, carheight, curbweight, enginesize, 
                           boreratio, stroke, compressionratio, horsepower, peakrpm, citympg,
                           highwaympg, price, PWR, Torque)

Corl <- cor(CarPrice_Numeric)
corrplot(Corl, type = "lower", method = "number", number.cex = .7)


####################################################################################################
###################################### Create Dummy varibales ######################################
### First Convert all binary categorical variables
#1. fueltype - Binary 
#2. aspiration - Binary
#3. enginelocation -Binary
#4. doornumber - Binary

#1. fueltype
CarPrice$fueltype <- as.factor(CarPrice$fueltype)
summary(CarPrice$fueltype)
levels(CarPrice$fueltype) <- c(0,1)
summary(CarPrice$fueltype)
CarPrice$fueltype <- as.numeric(levels(CarPrice$fueltype))[CarPrice$fueltype]
summary(CarPrice$fueltype)

#2. aspiration
CarPrice$aspiration <- as.factor(CarPrice$aspiration)
summary(CarPrice$aspiration)
levels(CarPrice$aspiration) <- c(0,1)
summary(CarPrice$aspiration)
CarPrice$aspiration <- as.numeric(levels(CarPrice$aspiration))[CarPrice$aspiration]
summary(CarPrice$aspiration)

#3. enginelocation
CarPrice$enginelocation <- as.factor(CarPrice$enginelocation)
summary(CarPrice$enginelocation)
levels(CarPrice$enginelocation) <- c(0,1)
summary(CarPrice$enginelocation)
CarPrice$enginelocation <- as.numeric(levels(CarPrice$enginelocation ))[CarPrice$enginelocation ]
summary(CarPrice$enginelocation)

#4. Doornumber
CarPrice$doornumber <- as.factor(CarPrice$doornumber)
summary(CarPrice$doornumber)
levels(CarPrice$doornumber) <- c(0,1)
summary(CarPrice$doornumber)
CarPrice$doornumber <- as.numeric(levels(CarPrice$doornumber))[CarPrice$doornumber]
summary(CarPrice$doornumber)

########### creating levels for Dummy variable: Symboling ##################

summary(CarPrice$symboling)
CarPrice$symboling <- as.factor(CarPrice$symboling)
summary(CarPrice$symboling)
# safe, moderate, risky

levels(CarPrice$symboling)[1:2] <- "safe"
str(CarPrice$symboling)
summary(CarPrice$symboling)

levels(CarPrice$symboling)[2:3] <- "moderate"
str(CarPrice$symboling)
summary(CarPrice$symboling)

levels(CarPrice$symboling)[3:4] <- "risky"
str(CarPrice$symboling)
summary(CarPrice$symboling)

##################################################################################################
##################### Convert Multi Categorical variables to dummy variables #####################
#5. Symboling
#6. enginetype
#7. carbody
#8. drivewheel
#9. fuelsystem
#10. CarCompany
#11. CylinderNumber

#4. Symboling
CarPrice$symboling <- as.factor(CarPrice$symboling)
summary(CarPrice$symboling)
dummy_0 <- data.frame(model.matrix(~symboling, data = CarPrice))
dummy_0 <- dummy_0[,-1]
colnames(CarPrice)
CarPrice_1 <- cbind(CarPrice[,-2], dummy_0)


#5. enginetype
CarPrice$enginetype <- as.factor(CarPrice$enginetype)
summary(CarPrice$enginetype)
dummy_1 <- data.frame(model.matrix(~enginetype, data = CarPrice_1))
dummy_1 <- dummy_1[,-1]
colnames(CarPrice_1)

CarPrice_1 <- cbind(CarPrice_1[,-14], dummy_1)

#6. carbody
CarPrice$carbody <- as.factor(CarPrice$carbody)
summary(CarPrice$carbody)
dummy_2 <- data.frame(model.matrix(~carbody, data = CarPrice_1))
dummy_2 <- dummy_2[,-1]
colnames(CarPrice_1)

CarPrice_1 <- cbind(CarPrice_1[,-6], dummy_2)

#7. drivewheel
CarPrice$drivewheel <- as.factor(CarPrice$drivewheel)
summary(CarPrice$drivewheel)
dummy_3 <- data.frame(model.matrix(~drivewheel, data = CarPrice_1))
dummy_3 <- dummy_3[,-1]
colnames(CarPrice_1)

CarPrice_1 <- cbind(CarPrice_1[,-6], dummy_3)

#8. fuelsystem
CarPrice$fuelsystem <- as.factor(CarPrice$fuelsystem)
summary(CarPrice$fuelsystem)
dummy_4 <- data.frame(model.matrix(~fuelsystem, data = CarPrice_1))
dummy_4 <- dummy_4[,-1]
colnames(CarPrice_1)

CarPrice_1 <- cbind(CarPrice_1[,-14], dummy_4)

#9. Car_Company 
CarPrice$Car_Company <- as.factor(CarPrice$Car_Company)
summary(CarPrice$Car_Company)
dummy_5 <- data.frame(model.matrix(~Car_Company, data = CarPrice_1))
dummy_5 <- dummy_5[-1]
colnames(CarPrice_1)

CarPrice_1 <- cbind(CarPrice_1[,-2], dummy_5)

#10. Cylinder Number
CarPrice$cylindernumber <- as.factor(CarPrice$cylindernumber)
summary(CarPrice$cylindernumber)
dummy_6 <- data.frame(model.matrix(~cylindernumber, data = CarPrice_1))
dummy_6 <- dummy_6[-1]
colnames(CarPrice_1)

CarPrice_1 <- cbind(CarPrice_1[,-11], dummy_6)

CarPrice_1$car_ID <- NULL

# Correclation between all the variables 
CorCP <- cor(CarPrice_1)
corrplot(CorCP, type = "lower", method = "color")

#################################################################################
############################### Modelling #######################################
#################################################################################

# Now let us start building the model.
# First divide the data in to training and test data
sum(is.na(CarPrice_1))

set.seed(100)

train_indecies <- sample(1:nrow(CarPrice_1), 0.7*nrow(CarPrice_1))
length(train_indecies)
train_data <- CarPrice_1[train_indecies,]
test_data <- CarPrice_1[-train_indecies,]


model_1 <- lm(price~., data = train_data)
summary(model_1)

# Using stepAIC Method: 
library(MASS)

step <- stepAIC(model_1, direction = "both")
step

model_2 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + peakrpm + PWR + Torque + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelfwd + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + Car_CompanyBmw + Car_CompanyBuick + 
                Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                Car_CompanyJaguar + Car_CompanyMazda + Car_CompanyMercury + 
                Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_2)
# For model_2:
# Multiple R-squared:  0.9768,	Adjusted R-squared:  0.9684
vif(model_2)

# variables with a high VIF or multicollinearity may be statistically significant (***) or p<0.05, 
# So, in this case you will first check for other insignificant variables before removing the variables with a higher VIF and lower p-values.
# Thus, we have to check and remove variables with a high VIF after checking for p-values

# Therefore in present model, Car_CompanyJaguar has high p-value = 0.216043
# which indicates Car_CompanyJaguar is less significant variable.
# Eliminate Car_CompanyJaguar and create new model

model_3 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + peakrpm + PWR + Torque + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelfwd + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + Car_CompanyBmw + Car_CompanyBuick + 
                Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                Car_CompanyMazda + Car_CompanyMercury + 
                Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_3)
# For model_3:
# Multiple R-squared:  0.9765,	Adjusted R-squared:  0.9682
vif(model_3)

# carbodyhardtop is the variable having high VIF value = 3.848039 and high p-value = 0.22201

model_4 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + peakrpm + PWR + Torque + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelfwd + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + Car_CompanyBmw + Car_CompanyBuick + 
                Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                Car_CompanyMazda + Car_CompanyMercury + 
                Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_4)
# For model_4:
# Multiple R-squared:  0.9762,	Adjusted R-squared:  0.9681
vif(model_4)

# wheelbase is the variable having high VIF value = 9.183761 and high p-value = 0.118106

model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + peakrpm + PWR + Torque + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelfwd + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + Car_CompanyBmw + Car_CompanyBuick + 
                Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                Car_CompanyMazda + Car_CompanyMercury + 
                Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_5)
# For model_5:
# Multiple R-squared:  0.9756,	Adjusted R-squared:  0.9676
vif(model_5)

# carbodysedan is the variable having high VIF value = 6.520189 and high p-value = 0.121709

model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + peakrpm + PWR + Torque + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + carbodyhatchback + 
                carbodywagon + drivewheelfwd + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + Car_CompanyBmw + Car_CompanyBuick + 
                Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                Car_CompanyMazda + Car_CompanyMercury + 
                Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_6)
# For model_6:
# Multiple R-squared:  0.975,	Adjusted R-squared:  0.9672
vif(model_6)

# Other variable with with high VIF are statistically significant (***) or p<0.05
# carbodyhatchback is the variable having high p-value = 0.629026 i.e. less significant

model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + peakrpm + PWR + Torque + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor +
                carbodywagon + drivewheelfwd + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + Car_CompanyBmw + Car_CompanyBuick + 
                Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                Car_CompanyMazda + Car_CompanyMercury + 
                Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_7)
# For model_7:
# Multiple R-squared:  0.975,	Adjusted R-squared:  0.9674
vif(model_7)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# enginetyperotor is the variable having high p-value = 0.109402 i.e. less significant

model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + peakrpm + PWR + Torque + enginetypel + enginetypeohc + 
                enginetypeohcf +
                carbodywagon + drivewheelfwd + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + Car_CompanyBmw + Car_CompanyBuick + 
                Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                Car_CompanyMazda + Car_CompanyMercury + 
                Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_8)
# For model_8:
# Multiple R-squared:  0.9744,	Adjusted R-squared:  0.9669
vif(model_8)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# enginetypeohc is the variable having VIF value = 9.692947 and p-value = 0.014356 i.e. less significant
      
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + peakrpm + PWR + Torque + enginetypel + 
                enginetypeohcf +
                carbodywagon + drivewheelfwd + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + Car_CompanyBmw + Car_CompanyBuick + 
                Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                Car_CompanyMazda + Car_CompanyMercury + 
                Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_9)
# For model_9:
# Multiple R-squared:  0.973,	Adjusted R-squared:  0.9654
vif(model_9)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# fuelsystemspdi is the variable having high p-value = 0.100888 i.e. less significant

model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + peakrpm + PWR + Torque + enginetypel + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + fuelsystem2bbl + 
                 fuelsystemmpfi + Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                 Car_CompanyMazda + Car_CompanyMercury + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_10)
# For model_10:
# Multiple R-squared:  0.9723,	Adjusted R-squared:  0.9649
vif(model_10)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# fuelsystemmpfi is the variable having high p-value = 0.090644 i.e. less significant

model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + peakrpm + PWR + Torque + enginetypel + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + fuelsystem2bbl + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                 Car_CompanyMazda + Car_CompanyMercury + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_11)
# For model_11:
# Multiple R-squared:  0.9766,	Adjusted R-squared:  0.9695
vif(model_11)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# fuelsystem2bbl high p-value = 0.130510 i.e. less significant

model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + peakrpm + PWR + Torque + enginetypel + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                 Car_CompanyMazda + Car_CompanyMercury + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_12)
# For model_12:
# Multiple R-squared:  0.971,	Adjusted R-squared:  0.9639
vif(model_12)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# aspiration is the least significant variable in present model

model_13 <- lm(formula = price ~ enginelocation + 
                 carwidth + peakrpm + PWR + Torque + enginetypel + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                 Car_CompanyMazda + Car_CompanyMercury + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_13)
# For model_13:
# Multiple R-squared:   0.97,	Adjusted R-squared:  0.963
vif(model_13)

# model_13 is the model having all the significant variable i.e. all the variable with p < 0.05.
# We can stop here but we can still move ahead to get more precise model.
# So we have to looks for variable that are less significant with star(**) and has higher VIF value.
# carwidth is the variable with higher VIF value = 7.194316
  
model_14 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque + enginetypel + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                 Car_CompanyMazda + Car_CompanyMercury + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + Car_CompanyVolvo + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_14)
# For model_14:
# Multiple R-squared:  0.9679,	Adjusted R-squared:  0.9607
vif(model_14)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyVolvo is the least significant variable in present model

model_15 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque + enginetypel + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                 Car_CompanyMazda + Car_CompanyMercury + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_15)
# For model_15:
# Multiple R-squared:  0.9663,	Adjusted R-squared:  0.9591
vif(model_15)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyMercury is the least significant variable in present model

model_16 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque + enginetypel + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + Car_CompanyIsuzu + 
                 Car_CompanyMazda + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_16)
# For model_16:
# Multiple R-squared:  0.9647,	Adjusted R-squared:  0.9576
vif(model_16)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyIsuzu is the least significant variable in present model

model_17 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque + enginetypel + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMazda + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanySaab + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_17)
# For model_17:
# Multiple R-squared:  0.9647,	Adjusted R-squared:  0.9576
vif(model_17)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanySaab is the least significant variable in present model

model_18 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque + enginetypel + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMazda + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train_data)

summary(model_18)
# For model_18:
# Multiple R-squared:  0.9608,	Adjusted R-squared:  0.9536
vif(model_18)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# cylindernumbersix is the least significant variable in present model

model_19 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque + enginetypel + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMazda + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + cylindernumberfive + 
                 cylindernumberfour, data = train_data)

summary(model_19)
# For model_19:
# Multiple R-squared:  0.9574,	Adjusted R-squared:   0.95
vif(model_19)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# enginetypel is the least significant variable in present model

model_20 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMazda + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + cylindernumberfive + 
                 cylindernumberfour, data = train_data)

summary(model_20)
# For model_20:
# Multiple R-squared:  0.9553,	Adjusted R-squared:  0.9479
vif(model_20)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# cylindernumberfour is the least significant variable in present model

model_21 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMazda + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanyToyota + 
                 Car_CompanyVolkswagen + cylindernumberfive, data = train_data)

summary(model_21)
# For model_21:
# Multiple R-squared:  0.9528,	Adjusted R-squared:  0.9455
vif(model_21)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# cylindernumberfive is the least significant variable in present model

model_22 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque + 
                 enginetypeohcf +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMazda + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanyToyota + 
                 Car_CompanyVolkswagen, data = train_data)

summary(model_22)
# For model_22:
# Multiple R-squared:  0.9519,	Adjusted R-squared:  0.9449
vif(model_22)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# enginetypeohcf is the least significant variable in present model

model_23 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMazda + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanyToyota + 
                 Car_CompanyVolkswagen, data = train_data)

summary(model_23)
# For model_23:
# Multiple R-squared:  0.9505,	Adjusted R-squared:  0.9438
vif(model_23)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyMazda is the least significant variable in present model

model_24 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMitsubishi + Car_CompanyNissan + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanyToyota + 
                 Car_CompanyVolkswagen, data = train_data)

summary(model_24)
# For model_24:
# Multiple R-squared:  0.9499,	Adjusted R-squared:  0.9435
vif(model_24)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyNissan is the least significant variable in present model

model_25 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMitsubishi + Car_CompanyPlymouth + 
                 Car_CompanyRenault + Car_CompanyToyota + 
                 Car_CompanyVolkswagen, data = train_data)

summary(model_25)
# For model_25:
# Multiple R-squared:  0.9488,	Adjusted R-squared:  0.9428
vif(model_25)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyRenault is the least significant variable in present model

model_26 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMitsubishi + Car_CompanyPlymouth + 
                 Car_CompanyToyota + 
                 Car_CompanyVolkswagen, data = train_data)

summary(model_26)
# For model_26:
# Multiple R-squared:  0.9478,	Adjusted R-squared:  0.9421
vif(model_26)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyVolkswagen is the least significant variable in present model

model_27 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMitsubishi + Car_CompanyPlymouth + 
                 Car_CompanyToyota, data = train_data)

summary(model_27)
# For model_27:
# Multiple R-squared:  0.9468,	Adjusted R-squared:  0.9415
vif(model_27)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyToyota is the least significant variable in present model

model_28 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyDodge + Car_CompanyHonda + 
                 Car_CompanyMitsubishi + Car_CompanyPlymouth, data = train_data)

summary(model_28)
# For model_28:
# Multiple R-squared:  0.9458,	Adjusted R-squared:  0.9408
vif(model_28)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyDodge is the least significant variable in present model

model_29 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyHonda + 
                 Car_CompanyMitsubishi + Car_CompanyPlymouth, data = train_data)

summary(model_29)
# For model_29:
# Multiple R-squared:  0.9444,	Adjusted R-squared:  0.9397
vif(model_29)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyPlymouth is the least significant variable in present model

model_30 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyHonda + 
                 Car_CompanyMitsubishi, data = train_data)

summary(model_30)
# For model_30:
# Multiple R-squared:  0.9423,	Adjusted R-squared:  0.938
vif(model_30)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyHonda is the least significant variable in present model

model_31 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick + 
                 Car_CompanyMitsubishi, data = train_data)

summary(model_31)
# For model_31:
# Multiple R-squared:  0.9404,	Adjusted R-squared:  0.9364
vif(model_31)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# Car_CompanyMitsubishi is the least significant variable in present model

model_32 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque +
                 carbodywagon + drivewheelfwd + 
                 Car_CompanyBmw + Car_CompanyBuick, data = train_data)

summary(model_32)
# For model_32:
# Multiple R-squared:  0.9375,	Adjusted R-squared:  0.9337 
vif(model_32)

# Variables with with higher VIF are statistically significant (***) or p<0.05
# drivewheelfwd is the least significant variable in present model

model_33 <- lm(formula = price ~ enginelocation + 
                 peakrpm + PWR + Torque +
                 carbodywagon + 
                 Car_CompanyBmw + Car_CompanyBuick, data = train_data)

summary(model_33)
# For model_33:
# Multiple R-squared:  0.9345,	Adjusted R-squared:  0.9311 
vif(model_33)
plot(model_33)

# Finally we got a model with all values as significant values with *** stars
# There are few variables with higher VIF value but with high significance as well.

# We have achieved a model in which all the variables are significant 
# These variables are: 
## enginelocation
## peakrpm
## PWR
## Torque
## carbodywagon
## Car_CompanyBmw
## Car_CompanyBuick

# So model_33 is the final model.

# Predicting the results using the test data.

Predict_1 <- predict(model_33, test_data[,-18]) 
test_data$Predicted_Price <- Predict_1

# Check the correlation and R-squared value
r <- cor(test_data$price, test_data$Predicted_Price) 					## 0.9225142
r_squared <- (cor(test_data$price, test_data$Predicted_Price))^2 		## 0.8510325
r_squared

plot(test_data$price,type = "l",col="red")
lines(test_data$Predicted_Price ,type = "l",col="blue") 
#### Both plots look similar







