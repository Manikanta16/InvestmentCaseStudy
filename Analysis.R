############ INVESTMENT CASE STUDY - 2/4/2018
#### Group Members
# Manikandan Kulanghat
# Santhana Krishnan
# Gaurav Jain
# Guru Manikanta Innamuri

########################################
# Checkpoint 1: Data Cleaning 1
########################################
#setting working directory
#setwd("C:\\SK\\DataScience\\ActualProgram\\0005_InvestmentCaseStudy\\InvestmentCaseStudy_R")
# Read file
companies <- read.delim("companies.txt", header = TRUE, stringsAsFactors = F)
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = F)
mapping <- read.csv("mapping.csv", stringsAsFactors = F)

# Prep file for merge
rounds2$company_permalink <- tolower(rounds2$company_permalink)
companies$permalink <- tolower(companies$permalink)
companies$permalink <- factor(companies$permalink)
summary(companies$permalink)
# Confirmed that permalink is the unique key for merging the companies.txt & rounds2.csv

# Unique companies
unique_rounds2_company <- unique(rounds2$permalink)

# Merge files
colnames(rounds2)[1] <- "permalink"
master_frame <- merge(companies,rounds2, by = "permalink")
diff <- setdiff(rounds2$permalink,companies$permalink)
unique_company <- unique(master_frame$permalink) #Confirm there is no data loss.

########################################
# Checkpoint 2 : Funding Type Analysis 
########################################

# install.packages("readxl")
library(readxl)

# Read excel data from the file Country Code - Language mapping.xlsx. This file is currently available in input directory. Please refer the same. 
Country_Code_Language_mapping <- read_excel("Country Code - Language mapping.xlsx",sheet = "Curated with UN file")

# Rename & Merge - Left outer to retain all the records in master_frame. 
colnames(Country_Code_Language_mapping)[3] <- "country_code"
master_frame <- merge(master_frame,Country_Code_Language_mapping, by = "country_code", all.x = TRUE )

# Funding type analysis. 
sum(is.na(master_frame$funding_round_type)) 
library(dplyr)

four_Fund <- filter(master_frame, funding_round_type == "venture" | funding_round_type == "angel" | funding_round_type == "seed" | funding_round_type == "private_equity" )
four_Fund
four_Fund_group <- group_by(four_Fund,funding_round_type)
four_Fund_group
invetment_Summary <- summarise(four_Fund_group, mean(raised_amount_usd, na.rm = T))
invetment_Summary

# investments on countries with english language before concluding the "venture" funding type as the result
invetment_Summary_all <- summarise(four_Fund_group, mean(raised_amount_usd, na.rm = T))
fourFund_group_english_all <- filter(four_Fund_group, Official_English == "Y")
invetment_Summary_english_all <- summarise(fourFund_group_english_all, mean(raised_amount_usd, na.rm = T))

########################################
# Checkpoint 3: Country Country Analysis
########################################

# [Optional] Funding Type analysis with restriction on english speaking countries. 
# fundType_group <- group_by(master_frame,funding_round_type)
# fourFund_group_english <- filter(fundType_group, Official_English == "Y",funding_round_type == "venture" | funding_round_type == "angel" | funding_round_type == "seed" | funding_round_type == "private_equity" )
# invetment_Summary_english <- summarise(fourFund_group_english, mean(raised_amount_usd, na.rm = T))

# group by the required fields & suymmarize the raised amount. 
country_group <- group_by(master_frame,country_code, `Country or Area`, funding_round_type)
country_group_english <- filter(country_group, country_code != "",  Official_English == "Y", !is.na(Official_English), funding_round_type == "venture")
country_group_total <- summarise(country_group_english, total_raised_amt = sum(raised_amount_usd, na.rm = T))

# library import for using arrange. 
library(plyr)
library(dplyr) # It was recommended to import the dplyr after plyr package. 

# top9 dataset creation. 
top9 <- head(arrange(country_group_total,desc(total_raised_amt)), n = 9)

# top 3 countries with investments
top_1_country <-top9$`Country or Area`[which.max(top9$total_raised_amt)]
top_2_country <- top9$`Country or Area`[which(top9$total_raised_amt == sort(top9$total_raised_amt, decreasing=T)[2])] 
top_3_country <- top9$`Country or Area`[which(top9$total_raised_amt == sort(top9$total_raised_amt, decreasing=T)[3])] 

#########################################
# Checkpoint 4: Country Country Analysis
########################################
# install.packages("tidyr")
library(tidyr)


# Splitting the category lust into primary_sector & sub_sectors.
master_frame_Seperated <- master_frame %>% separate(col = category_list, into = c("primary_sector","sub_sectors"),sep = "\\|",extra = "merge", fill = "left")

# Renaming the column for merge. 
colnames(mapping)[1] <- "primary_sector"

# Converting the null's to blank for easier merge with mapping dataframe. Note: Mapping dataframe has a seperate record for blank.
master_frame_Seperated$primary_sector[is.na(master_frame_Seperated$primary_sector)] <- ""

# Creation of the merged master frame with mapping dataframe. 
master_frame_wide <- merge(master_frame_Seperated,mapping,  by = "primary_sector", all.x = TRUE)

# verifying the # of unique funding_round_permalink
length(unique(master_frame_wide$funding_round_permalink))

#Intrim step in the conversion from wide to long data frame. 
master_frame_wide_intrim <- gather(data = master_frame_wide, key = main_sector, value = my_val, Automotive...Sports:Social..Finance..Analytics..Advertising)

# verifying the # of unique funding_round_permalink
length(unique(master_frame_wide_intrim$funding_round_permalink))

# Removal of reords that has 0 after conversion to long dataframe as they add no value in analysis. 
master_frame_long <- filter(master_frame_wide_intrim, master_frame_wide_intrim$my_val != 0)

# verifying the # of unique funding_round_permalink
length(unique(master_frame_long$funding_round_permalink))
unique(master_frame_wide_intrim$my_val) 
# this proves that there were primary categories that did not exist in the mapping file which got excluded in this conversion. 

# Removing the my_val column. 
master_frame_long$my_val <- NULL

########################################
# Checkpoint 5: Sector Analysis 2
########################################

# ----- Assuming that we are using the dataset d1, d2 & d3 with companies that received investments in the range of 5M to 15M as part of the funding as Spark investments is intrested in this range.
unique(master_frame$status)

options(scipen = 999) # disable scientific notation. 

d1 <- filter(master_frame_long, country_code == "USA" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000 & primary_sector != "") #filtering the investments in d1, d2, d3 by investment range to focus the business cirteria. 
d2 <- filter(master_frame_long, country_code == "GBR" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000 & primary_sector != "")
d3 <- filter(master_frame_long, country_code == "IND" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000 & primary_sector != "")

# Unique funding_round_permalink
length(unique(d1$funding_round_permalink))
length(unique(d2$funding_round_permalink))
length(unique(d3$funding_round_permalink))

# Total funding based on country. 
d1_total_raised <- sum(d1$raised_amount_usd)
d2_total_raised <- sum(d2$raised_amount_usd)
d3_total_raised <- sum(d3$raised_amount_usd)

# install.packages("sqldf")
library(sqldf)

# Identifying the sector(s) that received highest # on investments in the range of 5M-15M in funding. 
d1fund_count <- sqldf("select primary_Sector, count(funding_round_permalink) as no_of_investment from d1 group by primary_Sector order by count(funding_round_permalink) desc LIMIT 4;") # 0 competing counts between sectors.
d2fund_count <- sqldf("select primary_Sector, count(funding_round_permalink) as no_of_investment from d2 group by primary_Sector order by count(funding_round_permalink) desc LIMIT 5;") # 2 sectors having same count for 3rd position.
d3fund_count <- sqldf("select primary_Sector, count(funding_round_permalink) as no_of_investment from d3 group by primary_Sector order by count(funding_round_permalink) desc LIMIT 9;") # 3 sectors having same count for 2nd position & 4 sectors for 3rd position. 

# filtering the datasets to the get the top sector investment data in each country
d1flt1 <- filter(d1, primary_sector == "Advertising")
d2flt1 <- filter(d2, primary_sector == "Advertising")
d3flt1 <- filter(d3, primary_sector == "E-Commerce")

# filtering the datasets to the get the top second sector investment data in each country
d1flt2 <- filter(d1, primary_sector == "Biotechnology")
d2flt2 <- filter(d2, primary_sector == "E-Commerce")
d3flt2 <- filter(d3, primary_sector == "E-Automotive" | primary_sector == "Curated Web" | primary_sector == "Fashion")

# SQL query to get the companies receiving the highest funding in investments of 5M-15M in the top sector. This is extracted from the above filtered dataset. Limiting the results to 2 to make sure there are no more than one companies in the first spot. 
sqldf("select name, sum(raised_amount_usd) as investment_sum from d1flt1 group by name order by sum(raised_amount_usd) desc LIMIT 2")
sqldf("select name, sum(raised_amount_usd) as investment_sum from d2flt1 group by name order by sum(raised_amount_usd) desc LIMIT 2")
sqldf("select name, sum(raised_amount_usd) as investment_sum from d3flt1 group by name order by sum(raised_amount_usd) desc LIMIT 2")

# SQL query to get the companies receiving the highest funding in investments of 5M-15M in the top second sector. This is extracted from the above filtered dataset. Limiting the results to 2 to make sure there are no more than one companies in the first spot. 
sqldf("select name, sum(raised_amount_usd) as investment_sum from d1flt2 group by name order by sum(raised_amount_usd) desc LIMIT 2")
sqldf("select name, sum(raised_amount_usd) as investment_sum from d2flt2 group by name order by sum(raised_amount_usd) desc LIMIT 2")
sqldf("select name, sum(raised_amount_usd) as investment_sum from d3flt2 group by name order by sum(raised_amount_usd) desc LIMIT 2")

# This case study was really an eye-opener for working on R & Tableau. 