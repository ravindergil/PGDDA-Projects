# The Uber case study has been divided in to four check points
# Check point 1 : Data Preparation
# Check point 2 : Visually identify the frequency of requests in cancelled and No cars available status
# Check point 3 : Visually analysis Demand and Supply gap for: Airport to City & City to Airport
# Check point 4 : Result Analysis Recommendations and Conclusion

# Loading relevant packages:
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)

# Set Up directory:
setwd("C:\\Users\\Desktop\\UberCaseStudy")

# Load the data file and check the structure:
Uber <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)
str(Uber)
head(Uber)

# --------------------------------- Check Point 1 -------------------------------- #
# Data Prrparation: cleansing and formating
# Data Cleaning: Deriving metrics: Type Driven Metrics

Uber$Request.TimeStamp <- parse_date_time(x = Uber$Request.timestamp, 
                                 orders = c("d m Y H:M:S", "d m Y H:M"), 
                                 locale = "eng")

Uber$Drop.TimeStamp <- parse_date_time(x = Uber$Drop.timestamp, 
                                  orders = c("d m Y H:M:S", "d m Y H:M"), 
                                  locale = "eng")

# Remove column that are nor required
Uber$Request.timestamp <- NULL
Uber$Drop.timestamp <- NULL


# Sorting Data based on Driver.id and Request Time
Uber <- Uber[order(Uber$Driver.id, Uber$Request.TimeStamp),]

# Triptime:
Uber$Trip_Time <- round(as.numeric(Uber$Drop.TimeStamp - Uber$Request.TimeStamp), digits = 2)
Average_Trip_Time <- round(mean(Uber$Trip_Time, na.rm = TRUE), digits = 2)

# Deriving new metric of Type date and time from Request.TimeStamp and Drop.TimeStamp 
Uber <- separate(Uber, Request.TimeStamp, into = c("Request_Date", "Request_Time"), sep = " ")
Uber <- separate(Uber, Drop.TimeStamp, into = c("Drop_Date", "Drop_Time"), sep = " ")

#Further extracting hour from Request_Time and Drop_Time Column
Uber$Request_Hour <- substring(Uber$Request_Time,0,2)
Uber$Drop_Hour <- substring(Uber$Drop_Time,0,2)

# Changing measures to catagorical variables
Uber$Status <- as.factor(Uber$Status)
Uber$Pickup.point <- as.factor(Uber$Pickup.point)
Uber$Driver.id <- as.factor(Uber$Driver.id)
Uber$Request_Hour <- as.factor(Uber$Request_Hour)
Uber$Drop_Hour <- as.factor(Uber$Drop_Hour)

# Plot depicting amount of requests split status wise for each hour.
Uber_plot1 <- ggplot(Uber, aes(x = Request_Hour, fill = Status)) + 
  geom_bar(alpha = 0.75, position = position_dodge(0.75)) + labs(title = "Supply Demand trend over the whole Time") +
  xlab("Request Hour")  + ylab("Number of Requests") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,300) 
Uber_plot1
  
# --------------------------------- Check Point 2 -------------------------------- #
# Check frequency of requests that get cancelled or show 'no cars available' from airport to city request

## Case:1 Airport to City
# -----------------------

Uber_cancel_Airport <- filter(Uber, Uber$Pickup.point == "Airport", !Uber$Status == "Trip Completed")
Airport_Cancel <- ggplot(Uber_cancel_Airport, aes(x = Request_Hour, fill = Status)) + 
  geom_bar(position = "dodge") + labs(title = "From Airport to City") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,300)
Airport_Cancel

## Case:2 City to Airport
# -----------------------
# Check frequency of requests that get cancelled or show 'no cars available' from city to airport request
  
Uber_Cancel_City <- filter(Uber, Uber$Pickup.point == "City", !Uber$Status == "Trip Completed")
City_cancel <- ggplot(Uber_Cancel_City, aes(x = Request_Hour, fill = Status)) + 
    geom_bar(position = "dodge") + labs(title = "From City to Airport") +
    theme(plot.title = element_text(hjust = 0.5)) + ylim(0,300)
City_cancel
  
# Combine both plots
grid.arrange(Airport_Cancel, City_cancel, nrow = 1, ncol = 2, 
            top = "Frequency of requests that get Cancelled or show No Cars Available")


# --------------------------------- Check Point 3 -------------------------------- #
# Demand and Supply Check

# Checking for Cab demands from Airport to City and from City to Airport by the cutomers.

# Request for Pick Up from air port i.e. Demand
# This will include Completed, Cancelled as well as requests with no cars available status.

Uber_Airport_Request <- filter(Uber, Uber$Pickup.point == "Airport")
Uber_Airport_Request_11 <- filter(Uber_Airport_Request, Uber_Airport_Request$Request_Date == "2016-07-11")
Uber_Airport_Request_12 <- filter(Uber_Airport_Request, Uber_Airport_Request$Request_Date == "2016-07-12")
Uber_Airport_Request_13 <- filter(Uber_Airport_Request, Uber_Airport_Request$Request_Date == "2016-07-13")
Uber_Airport_Request_14 <- filter(Uber_Airport_Request, Uber_Airport_Request$Request_Date == "2016-07-14")
Uber_Airport_Request_15 <- filter(Uber_Airport_Request, Uber_Airport_Request$Request_Date == "2016-07-15")

Demand_Airport_11 <- ggplot(Uber_Airport_Request_11, aes(x = Request_Hour)) + 
  geom_bar(fill = "dark green") + labs(title ="Demand at Airport on 11 July 2016") +
  theme(plot.title = element_text(hjust = 0.5))+ylim(0,100)
Demand_Airport_11

Demand_Airport_12 <- ggplot(Uber_Airport_Request_12, aes(x = Request_Hour)) + 
  geom_bar(fill = "dark green") + labs(title ="Demand at Airport on 12 July 2016") + 
  theme(plot.title = element_text(hjust = 0.5))+ylim(0,100)
Demand_Airport_12

Demand_Airport_13 <- ggplot(Uber_Airport_Request_13, aes(x = Request_Hour)) + 
  geom_bar(fill = "dark green") + labs(title ="Demand at Airport on 13 July 2016") +
  theme(plot.title = element_text(hjust = 0.5))+ylim(0,100)
Demand_Airport_13

Demand_Airport_14 <- ggplot(Uber_Airport_Request_14, aes(x = Request_Hour)) + 
  geom_bar(fill = "dark green") + labs(title ="Demand at Airport on 14 July 2016") + 
  theme(plot.title = element_text(hjust = 0.5))+ylim(0,100)
Demand_Airport_14

Demand_Airport_15 <- ggplot(Uber_Airport_Request_15, aes(x = Request_Hour)) + 
  geom_bar(fill = "dark green") + labs(title ="Demand at Airport on 15 July 2016") + 
  theme(plot.title = element_text(hjust = 0.5))+ylim(0,100)
Demand_Airport_15

# Compare Demands across all days
grid.arrange(Demand_Airport_11, Demand_Airport_12, Demand_Airport_13, Demand_Airport_14, Demand_Airport_15, 
           nrow = 2, ncol = 3, top="Demand at Airport each day")


# Over all Request for Pick Up from air port across all days:
Demand_Airport <- 
ggplot(Uber_Airport_Request, aes(x = Request_Hour)) +
  geom_bar(fill = "dark green") + labs(title = "Demand at Airport across all days") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,500)
Demand_Airport

# Response to the request for Pick Up from air port i.e. Supply
# This will only include Completed trips. Cancelled and requests with no cars available status will not be considered.
# As these cases will be considered as failure in making supply to the demand.

Uber_Airport_Response <- filter(Uber, Uber$Pickup.point == "Airport", Uber$Status == "Trip Completed")
Uber_Airport_Response_11 <- filter(Uber_Airport_Response, Uber_Airport_Response$Drop_Date == "2016-07-11")
Uber_Airport_Response_12 <- filter(Uber_Airport_Response, Uber_Airport_Response$Drop_Date == "2016-07-12")
Uber_Airport_Response_13 <- filter(Uber_Airport_Response, Uber_Airport_Response$Drop_Date == "2016-07-13")
Uber_Airport_Response_14 <- filter(Uber_Airport_Response, Uber_Airport_Response$Drop_Date == "2016-07-14")
Uber_Airport_Response_15 <- filter(Uber_Airport_Response, Uber_Airport_Response$Drop_Date == "2016-07-15")

Supply_Airport_11 <- 
ggplot(Uber_Airport_Response_11, aes(x = Drop_Hour)) + 
  geom_bar(fill = "red") + labs(title = "Supply at Airport on 11 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)

Supply_Airport_12 <-  
ggplot(Uber_Airport_Response_12, aes(x = Drop_Hour)) +
  geom_bar(fill = "red") + labs(title = "Supply at Airport on 12 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)

Supply_Airport_13 <-
ggplot(Uber_Airport_Response_13, aes(x = Drop_Hour)) +
  geom_bar(fill = "red") + labs(title = "Supply at Airport on 13 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)
  
Supply_Airport_14 <-
ggplot(Uber_Airport_Response_14, aes(x = Drop_Hour)) +
  geom_bar(fill = "red") + labs(title = "Supply at Airport on 14 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)
  
Supply_Airport_15 <-
ggplot(Uber_Airport_Response_15, aes(x = Drop_Hour)) + 
  geom_bar(fill = "red") + labs(title = "Supply at Airport on 15 July 2016") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)

# Compare Supply across all days
grid.arrange(Supply_Airport_11, Supply_Airport_12, Supply_Airport_13, Supply_Airport_14, Supply_Airport_15, 
             nrow = 2 , ncol = 3, top = "Supply at Airport each day")

# Over all Supply for Pick Up Demand from air port across all days:
Supply_Airport <- 
ggplot(Uber_Airport_Response, aes(x = Drop_Hour)) +
  geom_bar(fill = "red") + labs(title = "Supply at Airport across all days") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,500)

# Compare Demand and Supply each day:
grid.arrange(Demand_Airport_11, Demand_Airport_12, Demand_Airport_13, Demand_Airport_14, Demand_Airport_15, 
             Supply_Airport_11, Supply_Airport_12, Supply_Airport_13, Supply_Airport_14, Supply_Airport_15,
             nrow = 2, ncol = 5, top = "Gap in Demand and Supply from Airport to City each day")

# Compare over all Demand and Supply:
grid.arrange(Demand_Airport, Supply_Airport, nrow = 1, ncol = 2, 
             top = "Over all gap in Demand and Supply from Airport to City")

# Request for Pick Up from City i.e. Demand
# This will include Completed, Cancelled as well as requests with no cars available status.

Uber_City_Request <- filter(Uber, Uber$Pickup.point == "City")
Uber_City_Request_11 <- filter(Uber_City_Request, Uber_City_Request$Request_Date == "2016-07-11")
Uber_City_Request_12 <- filter(Uber_City_Request, Uber_City_Request$Request_Date == "2016-07-12")
Uber_City_Request_13 <- filter(Uber_City_Request, Uber_City_Request$Request_Date == "2016-07-13")
Uber_City_Request_14 <- filter(Uber_City_Request, Uber_City_Request$Request_Date == "2016-07-14")
Uber_City_Request_15 <- filter(Uber_City_Request, Uber_City_Request$Request_Date == "2016-07-15")

Demand_City_11 <- ggplot(Uber_City_Request_11, aes(x = Request_Hour)) +
  geom_bar(fill = "dark green") + labs(title = "Demand at City on 11 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)
Demand_City_11

Demand_City_12 <- ggplot(Uber_City_Request_12, aes(x = Request_Hour)) +
  geom_bar(fill = "dark green") + labs(title = "Demand at City on 12 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)
Demand_City_12

Demand_City_13 <- ggplot(Uber_City_Request_13, aes(x = Request_Hour)) +
  geom_bar(fill = "dark green") + labs(title = "Demand at City on 13 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)
Demand_City_13

Demand_City_14 <- ggplot(Uber_City_Request_14, aes(x = Request_Hour)) +
  geom_bar(fill = "dark green") + labs(title = "Demand at City on 14 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)
Demand_City_14

Demand_City_15 <- ggplot(Uber_City_Request_15, aes(x = Request_Hour)) +
  geom_bar(fill = "dark green") + labs(title = "Demand at City on 15 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)
Demand_City_15

# Compare Demands across all days
grid.arrange(Demand_City_11, Demand_City_12, Demand_City_13, Demand_City_14, Demand_City_15, 
             nrow = 2, ncol = 3, top = "Demand at City each day")

# Over all Request for Pick Up from air port across all days:
Demand_City <- ggplot(Uber_City_Request, aes(x = Request_Hour)) + 
  geom_bar(fill = "dark green") + labs(title = "Demand at City across all days") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,500)
Demand_City


# Response to the request for Pick Up from City i.e. Supply
# This will only include Completed trips. Cancelled and requests with no cars available status will not be considered.
# As these cases will be considered as failure in making supply to the demand.

Uber_City_Response <- filter(Uber, Uber$Pickup.point == "City", Uber$Status == "Trip Completed")
Uber_City_Response_11 <- filter(Uber_City_Response, Uber_City_Response$Request_Date == "2016-07-11")
Uber_City_Response_12 <- filter(Uber_City_Response, Uber_City_Response$Request_Date == "2016-07-12")
Uber_City_Response_13 <- filter(Uber_City_Response, Uber_City_Response$Request_Date == "2016-07-13")
Uber_City_Response_14 <- filter(Uber_City_Response, Uber_City_Response$Request_Date == "2016-07-14")
Uber_City_Response_15 <- filter(Uber_City_Response, Uber_City_Response$Request_Date == "2016-07-15")

Supply_City_11 <- ggplot(Uber_City_Response_11, aes(x = Request_Hour)) +
  geom_bar(fill = "red") + labs(title = "Supply at on 11 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0, 100)
Supply_City_11
  
Supply_City_12 <- ggplot(Uber_City_Response_12, aes(x = Request_Hour)) +
  geom_bar(fill = "red") + labs(title = "Supply at on 12 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)
Supply_City_12
  
Supply_City_13 <- ggplot(Uber_City_Response_13, aes(x = Request_Hour)) +
  geom_bar(fill = "red") + labs(title = "Supply at on 13 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)
Supply_City_13
  
Supply_City_14 <- ggplot(Uber_City_Response_14, aes(x = Request_Hour)) +
  geom_bar(fill = "red") + labs(title = "Supply at on 14 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)
Supply_City_14
  
Supply_City_15 <- ggplot(Uber_City_Response_15, aes(x = Request_Hour)) +
  geom_bar(fill = "red") + labs(title = "Supply at on 15 July 2016") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,100)
Supply_City_15
  
# Combining above plots 
grid.arrange(Supply_City_11, Supply_City_12, Supply_City_13, Supply_City_14, Supply_City_15,
             nrow = 2, ncol = 3, top = "Supply at City each day")

Supply_City <- ggplot(Uber_City_Response, aes(x = Request_Hour)) + 
  geom_bar(fill = "red") + labs(title = "Supply at City across all days") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,500)
Supply_City


# Combining Demand and Supply at City
grid.arrange(Demand_City, Supply_City, nrow = 1 , ncol = 2, 
             top = "Over all gap in Demand and Supply from City to airport")

# Comparing Demand and Supply from Airport to City and City to Airport
grid.arrange(Demand_Airport, Supply_Airport, Demand_City, Supply_City, 
             nrow = 1, ncol = 4,top = "Comparing Demand and Supply from Airport to City and City to Airport")



# Finding the top drivers who have cancelled and completed the most number of trips.

driver_uber_completed <- filter(Uber, Uber$Status == "Trip Completed")
driver_uber_completed_group <- group_by(driver_uber_completed, Driver.id)
driver_completed <- summarise(driver_uber_completed_group, length(Status))
driver_completed <- arrange(driver_completed, desc(`length(Status)`))


driver_uber_Cancelled <- filter(Uber, Uber$Status == "Cancelled")
driver_uber_Cancelled_group <- group_by(driver_uber_Cancelled, Driver.id)
driver_cancelled <- summarise(driver_uber_Cancelled_group, length(Status))
driver_cancelled <- arrange(driver_cancelled, desc(`length(Status)`))



