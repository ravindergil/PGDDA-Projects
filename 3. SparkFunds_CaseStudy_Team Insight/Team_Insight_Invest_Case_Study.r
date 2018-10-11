#Download the required Library if not already  present 

requiredPackages = c('dplyr','stringr','tidyr')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)){ 
    install.packages(p)
  }
  library(p,character.only = TRUE)
}

# creating and setting working directory
dir.create("Upgrade_case")
setwd("upgrade_case")

# downlaoding project data sets and other files
url_Companies <- "https://cdn.upgrad.com/UpGrad/temp/d934844e-5182-4b58-b896-4ba2a499aa57/companies.txt"
url_round2 <- "https://cdn.upgrad.com/UpGrad/temp/4c3b5ed0-e5dc-4838-89a2-173d8707d857/rounds2.csv"
url_mapping <- "https://cdn.upgrad.com/UpGrad/temp/231dc91c-0642-470d-a362-29ddcd7142ce/mapping.csv"

download.file(url_Companies, destfile="./companies.txt")
download.file(url_round2, destfile="./rounds2.csv")
download.file(url_mapping, destfile="./mapping.csv")

# Load the data from companies and rounds2 file.
companies <- read.delim("companies.txt", header =  TRUE , stringsAsFactors = FALSE)
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)

#load the mapping file and the missing mappings, mapping_missing_sectors is a CSV
# file that contains the mappings of the missing primary sectors from the mappings file provided.
mapping <- read.csv("mapping.csv", header =  TRUE, stringsAsFactors = FALSE)

# CHECKPOINT 1 *******************************************************************************************
#
#clean(convert to lower case) the permalink column in companies and rounds2 dataframe
companies$permalink<-tolower(companies$permalink)
rounds2$company_permalink<-tolower(rounds2$company_permalink)

  #How many unique companies are present in companies?
n_distinct(companies$permalink)    #Kaushal 
n_distinct(rounds2$company_permalink)

  #In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
# company_permalink
  
  # Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
  # NO
setdiff(rounds2$company_permalink, companies$permalink)

  #Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. 
  #Name the merged frame master_frame. How many observations are present in master_frame?
#114949
master_frame<-merge(companies, rounds2, by.x = "permalink", by.y = "company_permalink")
nrow(master_frame)
# CHECKPOINT 2 *******************************************************************************************

# checking the number of NA values in the column "raised_amount_usd" of master_frame
sum(is.na(master_frame$raised_amount_usd))


#1.	Calculate the average investment amount for each of the four 
#funding types (venture, angel, seed, and private equity)
master_frame[which(is.na(master_frame$raised_amount_usd)),"raised_amount_usd"]<-0

shares <- group_by(master_frame, funding_round_type)

fund_investment<-summarise(shares, 
          mean(raised_amount_usd, na.rm = T) )
colnames(fund_investment) <- c("fund_type", "avg_raised_amt")
arrange(fund_investment,desc(avg_raised_amt))
#By arranging the fund_investment we can clearly see that Venture is most suitable among the four 
# VENTURE        -  10,634,054
# ANGEL          - 764564
# SEED           - 556607
# PRIVATE EQUITY - 62,111,788

#2.	Based on the average investment amount calculated above, which investment type 
#do you think is the most suitable for Spark Funds

# VENTURE 

# CHECKPOINT 3 *******************************************************************************************

# 1. Spark Funds wants to see the top nine countries which have received the highest total funding 
# (across ALL sectors for the chosen investment type)


master_country_group <- filter(group_by(master_frame, country_code), funding_round_type == "venture")
master_country_group <- master_country_group [!(master_country_group$country_code==""),]
top9 <- summarise(master_country_group, sum(raised_amount_usd))
colnames(top9) <- c("Country_Code","Total_Sum")


# 2.	For the chosen investment type, make a data frame named top9 with the top nine countries 
# (based on the total investment amount each country has received)
top9 <- head(arrange(top9, desc(Total_Sum)),9)
top9
# 3. Identify the top three English-speaking countries in the data frame top9.
arrange(top9)
#As Chaina's main language is chaines then the Top 3 english speaking countries are :-
      #1.)USA
      #2.)GBR(Great Britan)
      #3.)IND(India) 


# CHECKPOINT 4 *******************************************************************************************

#1.	Extract the primary sector of each category list from the category_list column
#2.	Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors 
#(Note that 'Others' is also considered one of the main sectors)

#Primary Sector extract from category_list column
primary_sector_list <- str_split(master_frame$category_list, pattern="\\|")
primary_sector <- sapply(primary_sector_list, function(x) x[1][1])
master_frame[,"primary_sector"] <- primary_sector

#Converting the primary_sector and category_list columns to lowercase
master_frame$primary_sector <- str_to_lower(master_frame$primary_sector)
mapping$category_list <- str_to_lower(mapping$category_list)


mapping$Blanks<-NULL
new_mapping <- gather(mapping, main_sector, industry_val, 2:9)
new_mapping <- new_mapping[!(new_mapping$industry_val == 0),]
new_mapping <- new_mapping[, -3]



final_master<- merge(master_frame,new_mapping,by.x = "primary_sector",by.y="category_list")



# CHECKPOINT 5 *******************************************************************************************
#Creating the data frames for the 3 favourable english speaking countries and FT = venture

#subset the dataframe on the basis of country code,funding type and amount raised.
#Top three countries who received top investment count in venture funding with investment amount between
#5 and 15 million are subsetted to a dataframe
d1 <- filter(final_master,(final_master$country_code=="USA" & final_master$funding_round_type=="venture" & final_master$raised_amount_usd >= 5000000
                          &final_master$raised_amount_usd <=15000000))
d1$raised_amount_usd<-format(d1$raised_amount_usd, scientific=F)
d1$raised_amount_usd<-as.numeric(d1$raised_amount_usd)

f <- subset(final_master,(final_master$country_code=="GBR" & final_master$funding_round_type=="venture" & final_master$raised_amount_usd >= 5000000))
d2 <- subset(f,(f$raised_amount_usd <=15000000))
d2$raised_amount_usd<-format(d2$raised_amount_usd, scientific=F)
d2$raised_amount_usd<-as.numeric(d2$raised_amount_usd)
g <- subset(final_master,(final_master$country_code=="IND" & final_master$funding_round_type=="venture" & final_master$raised_amount_usd >=5000000))
d3 <- subset(g,(g$raised_amount_usd <=15000000))
d3$raised_amount_usd<-format(d3$raised_amount_usd, scientific=F)
d3$raised_amount_usd<-as.numeric(d3$raised_amount_usd)
#Count the number of investments in 08 main sectors in the country USA and save it
mainsec_investcount <- count(d1,main_sector)

#change the name of main sector column for merging 
names(mainsec_investcount)[1] <- "main_sector"

#merge the count of investment column with d1(USA) dataframe using left merge
d1 <- merge(d1,mainsec_investcount,by="main_sector",all.x=T)

#change the name of merged column for better understanding
names(d1)[18] <- "mainsec_investcount"

#Aggregate the invsetment amount in d1 dataframe on the basis of main sectors and save it
d1_mainsec_invest <- aggregate(d1$raised_amount_usd,by=list(d1$main_sector),FUN=sum)
# Change the name of columns of aggregated dataframe
names(d1_mainsec_invest)[1] <- "main_sector"
names(d1_mainsec_invest)[2] <- "mainsec_Totalinvestment"
#Merge the aggregated dataframe with d1 dataframe
d1 <- merge(d1,d1_mainsec_invest,by="main_sector",all.x=T)

#Count the number of investments in 08 main sectors in the country GBR and save it
mainsec2_investcount <- count(d2,main_sector)

#change the name of main sector column for merging
names(mainsec2_investcount)[1] <- "main_sector"

#merge the count of investment column with d2(GBR) dataframe using left merge
d2 <- merge(d2,mainsec2_investcount,by="main_sector",all.x=T)

#change the name of merged column for better understanding
names(d2)[18] <- "mainsec_investcount"

#Aggregate the invsetment amount in d2 dataframe on the basis of main sectors and save it
d2_mainsec_invest <- aggregate(d2$raised_amount_usd,by=list(d2$main_sector),FUN=sum)

# Change the name of columns of aggregated dataframe
names(d2_mainsec_invest)[1] <- "main_sector"
names(d2_mainsec_invest)[2] <- "mainsec_Totalinvestment"
#Merge the aggregated dataframe with d2 dataframe
d2 <- merge(d2,d2_mainsec_invest,by="main_sector",all.x=T)

#Count the number of investments in 08 main sectors in the country IND and save it
mainsec3_investcount <- count(d3,main_sector)
#change the name of main sector column for merging
names(mainsec3_investcount)[1] <- "main_sector"
#merge the count of investment column with d3(IND) dataframe using left merge
d3 <- merge(d3,mainsec3_investcount,by="main_sector",all.x=T)
#change the name of merged column for better understanding
names(d3)[18] <- "mainsec_investcount"
#Aggregate the invsetment amount in d3 dataframe on the basis of main sectors and save it
d3_mainsec_invest <- aggregate(d3$raised_amount_usd,by=list(d3$main_sector),FUN=sum)
# Change the name of columns of aggregated dataframe
names(d3_mainsec_invest)[1] <- "main_sector"
names(d3_mainsec_invest)[2] <- "mainsec_Totalinvestment"
#Merge the aggregated dataframe with d3 dataframe
d3 <- merge(d3,d3_mainsec_invest,by="main_sector",all.x=T)


write.csv(final_master,"final_master.csv")

#------------------------------------------------------------------------------------------------------------------------------------------
#code for answering questions in excel worksheet
# 1.)Total number of Investments in d1
checkPoint5_ques1 <- sum(mainsec_investcount$n)
checkPoint5_ques1

checkPoint5_ques1.1 <- sum(mainsec2_investcount$n)
checkPoint5_ques1.1

checkPoint5_ques1.2 <- sum(mainsec3_investcount$n)
checkPoint5_ques1.2
#--------------------------------------------------------------------------------------------
#2.)Total amount of Investment
checkPoint5_ques2 <- sum(d1_mainsec_invest$mainsec_Totalinvestment)
checkPoint5_ques2

checkPoint5_ques2.1 <- sum(d2_mainsec_invest$mainsec_Totalinvestment)
checkPoint5_ques2.1

checkPoint5_ques2.2 <- sum(d3_mainsec_invest$mainsec_Totalinvestment)
checkPoint5_ques2.2



#3.1 Top sector (based on count of investments)
topSector_D1<-arrange(d1_mainsec_invest,desc(d1_mainsec_invest$mainsec_Totalinvestment))
      topSector_D1
      #Others is the Top most sector 
      first_Sector_D1<-topSector_D1[1,1]
      first_Sector_D1
      #Cleantech..Semiconductor is the 2nd  top sectors
      second_Sector_D1<-topSector_D1[2,1]
      second_Sector_D1
      #Social..Finance..Analytics..Advertising is the 3rd top sector 
      third_Sector_D1<-topSector_D1[3,1]
      third_Sector_D1
#3.2
topSector_D2<-arrange(d2_mainsec_invest,desc(d2_mainsec_invest$mainsec_Totalinvestment))
      topSector_D2
      #Others is the top most sector
      first_Sector_D2<-topSector_D2[1,1]
      first_Sector_D2
      #Cleantech..Semiconductor is the 2nd  top sectors
      second_Sector_D2<-topSector_D2[2,1]
      second_Sector_D2
      #Social..Finance..Analytics..Advertising is the 3rd top sector
      third_Sector_D2<-topSector_D2[1,1]
      third_Sector_D2
#3.3
topSector_D3<-arrange(d3_mainsec_invest,desc(d3_mainsec_invest$mainsec_Totalinvestment))
      topSector_D3
      #Others is the top most sector
      first_Sector_D3<-topSector_D3[1,1]
      first_Sector_D3
      #News..Search.and.Messaging is the 2nd top sector
      second_Sector_D3<-topSector_D3[2,1]
      second_Sector_D3
      #Social..Finance..Analytics..Advertising is the 3rd top sector
      Third_Sector_D3<-topSector_D3[3,1]
      Third_Sector_D3
#Number of investments in the top sector 
      #For D1
top_investments_D1<-filter(mainsec_investcount,mainsec_investcount$main_sector=="Others")
top_investments_D1

second_investments_D1<-filter(mainsec_investcount,mainsec_investcount$main_sector=="Cleantech..Semiconductor")
second_investments_D1

third_investment_D1<-filter(mainsec_investcount,mainsec_investcount$main_sector=="Social..Finance..Analytics..Advertising")
third_investment_D1

    #For D2
top_investments_D2<-filter(mainsec2_investcount,mainsec2_investcount$main_sector=="Others")
top_investments_D2

second_investments_D2<-filter(mainsec2_investcount,mainsec2_investcount$main_sector=="Cleantech..Semiconductor")
second_investments_D2

third_investment_D2<-filter(mainsec2_investcount,mainsec2_investcount$main_sector=="Social..Finance..Analytics..Advertising")
third_investment_D2

    #For D3
top_investments_D3<-filter(mainsec3_investcount,mainsec3_investcount$main_sector=="Others")
top_investments_D3

second_investments_D3<-filter(mainsec3_investcount,mainsec3_investcount$main_sector=="News..Search.and.Messaging")
second_investments_D3

third_investment_D3<-filter(mainsec3_investcount,mainsec3_investcount$main_sector=="Social..Finance..Analytics..Advertising")
third_investment_D3
#_______________________________________________
#For the top sector count-wise (point 3), which company received the highest investment?
  #For country USA
#-----------------------------------------------------------------
summary_D1= d1 %>%
  group_by(main_sector)%>%
  summarise(sum_D1 = sum(raised_amount_usd, na.rm=TRUE),
            lth_D1 = length(raised_amount_usd)) %>%
  arrange(desc(sum_D1))

summary2_D1 = d1 %>%
  filter(main_sector  %in% c("Others", "Cleantech_Semiconductors" )) %>%
  group_by( main_sector,name)%>%
  summarise(sum_D1 = sum(raised_amount_usd, na.rm=TRUE),
            lth_D1 = length(raised_amount_usd)) %>%
  arrange( main_sector, desc(sum_D1)) 
  

 #For GBR
summary_D2= d2 %>%
  group_by(main_sector)%>%
  summarise(sum_D2 = sum(raised_amount_usd, na.rm=TRUE),
            lth_D2 = length(raised_amount_usd)) %>%
  arrange(desc(sum_D2))

   
summary2_D2 = d2 %>%
  filter(main_sector  %in% c("Others", "Cleantech_Semiconductors" )) %>%
  group_by( main_sector,name)%>%
  summarise(sum_D1 = sum(raised_amount_usd, na.rm=TRUE),
            lth_D1 = length(raised_amount_usd)) %>%
  arrange( main_sector, desc(sum_D1))

 #For India
summary_D3= d3 %>%
  group_by(main_sector)%>%
  summarise(sum_D3 = sum(raised_amount_usd, na.rm=TRUE),lth_D3 = length(raised_amount_usd)) %>%
  arrange(desc(sum_D3))

summary2_D3 = d3 %>%
  filter(main_sector  %in% c("Others", "News_Search_Messaging" )) %>%
  group_by( main_sector,name)%>%
  summarise(sum_D3 = sum(raised_amount_usd, na.rm=TRUE),
            lth_D3 = length(raised_amount_usd)) %>%
  arrange( main_sector, desc(sum_D3))

