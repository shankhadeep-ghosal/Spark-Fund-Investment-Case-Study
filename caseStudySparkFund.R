#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('stringr')
#install.packages('bindrcpp')

library(dplyr)
library(tidyr)
library(stringr)
library(bindrcpp)

#Checkpoint 1: Data Cleaning 1
################################

#?read.delim()
companies<-read.delim('companies.txt',header = TRUE,stringsAsFactors = FALSE)
#View(comapanies)
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)
#View(rounds2)

#Table 1.1
#Cleaning Companies and rounds2 dataset
companies <- mutate(companies, permalink1 = str_to_lower(companies$permalink))
companies <- companies[,-1]

rounds2 <- mutate(rounds2, permalink1 = str_to_lower(rounds2$company_permalink))
rounds2 <- rounds2[,-1]

# How many unique companies in companies, permalink1 is the primary_key for companies

n_distinct(companies$permalink1,na.rm = TRUE)#66368
length(unique(companies$permalink1))

# How many unique companies are present in rounds2?

?n_distinct
n_distinct(rounds2$permalink1,na.rm = TRUE)#66368
length(unique(rounds2$permalink1))#66368

# Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
setdiff(rounds2$permalink1, companies$permalink1)#Yes


# Merge the two data frames so that all variables (columns) 
# in the companies frame are added to the rounds2 data frame. 
# Name the merged frame master_frame. How many observations are present in master_frame?

master_frame <- inner_join(companies, rounds2, by = "permalink1")
nrow(master_frame)#114946

#Checkpoint 2: Funding Type Analysis
####################################

#TABLE2.1
#########

# checking the number of NA values in the column "raised_amount_usd" of master_frame
sum(is.na(master_frame$raised_amount_usd))

#replacing NA values with numeric 0
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0

#Grouping on Fund type, and calculating mean(raised_amount_type) 
master_fund_group <- group_by(master_frame, funding_round_type)
fund_investment_avg <- summarise(master_fund_group, mean(raised_amount_usd))
colnames(fund_investment_avg) <- c("Fund_Type", "Avg_Raised_Amount")


#Checkpoint 3: Country Analysis
###############################

#TABLE3.1
##########

#Spark Funds wants to see the top nine countries which have received 
#the highest total funding (across ALL sectors for the chosen investment type)

#For the chosen investment type, make a data frame named top9 
#with the top nine countries (based on the total investment amount each country has received)
#Group by country, and calculating mean(raised_amount_type) for Venture funding type.
master_country_group <- filter(group_by(master_frame, country_code), funding_round_type == "venture")
top9 <- summarise(master_country_group, sum(raised_amount_usd))
colnames(top9) <- c("Country_Cd","Total")
top9 <- head(arrange(top9, desc(Total)),9)


#Checkpoint 4: Sector Analysis 1
################################

 
#(Note that 'Others' is also considered one of the main sectors)

#Extract the primary sector of each category list from the category_list column

Primary_Seclist <- str_split(master_frame$category_list, pattern="\\|")
Primary_Sector <- sapply(Primary_Seclist, function(x) x[1][1])
master_frame[,"Primary_Sector"] <- Primary_Sector

#Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors 
Mapping <- read.csv("mapping.csv", header=TRUE, stringsAsFactors=FALSE)

#mapping_bkp,Backup of the mapping data frame
Mapping_Bkp <- Mapping
#master_frame_bkp,Backup of the master_frame
Master_Frame_Bkp <- master_frame

# replace "0" with "na" and "2.na" with "2.0" one after another
Mapping$category_list <- gsub("0", "na",Mapping$category_list)
Mapping$category_list <- gsub("2.na", "2.0", Mapping$category_list)

#Converting the primary_sector and category_list columns to lowercase
master_frame$Primary_Sector <- str_to_lower(master_frame$Primary_Sector)
Mapping$category_list <- str_to_lower(Mapping$category_list)

#Wide to long conversion of the mapping dataframe
Mapping_long <- gather(Mapping, main_sector, nval, 2:10)
Mapping_long <- Mapping_long[!(Mapping_long$nval == 0), ]
Mapping_long <- Mapping_long[,-3]

#Mapping_long_bkp,Backup of the mapping_long dataframe
Mapping_long_bkp <- Mapping_long

#Combine the original mappings file with the missing_mapping file
#Mapping_long <- rbind(mapping_long,missing_mappings)

#merge the Mapping_long and master_frame on primary sector
Final_Master_P <- merge(master_frame, Mapping_long, by.x = "Primary_Sector", by.y = "category_list")


#Checkpoint 5: Sector Analysis 2
################################

#Creating the data frames  for the 3 favourable english speaking countries and FT = venture

D1_india_investment <- filter(Final_Master_P, country_code == "IND", funding_round_type == "venture")
sum(D1_india_investment$raised_amount_usd)
D2_usa_investment <- filter(Final_Master_P, country_code == "USA", funding_round_type == "venture")
sum(D2_usa_investment$raised_amount_usd)
D3_gbr_investment <- filter(Final_Master_P, country_code == "GBR", funding_round_type == "venture")
sum(D3_gbr_investment$raised_amount_usd)
#create data frames with groupings on main sector
group_main_sector <- function(y)
{
  sector_group <- group_by(y, main_sector)
}

D1_india_invest_grp <- group_main_sector(D1_india_investment)
D2_usa_invest_grp <- group_main_sector(D2_usa_investment)
D3_gbr_invest_grp <- group_main_sector(D3_gbr_investment)


# Summarises the main sectors with Avg raised amount, number of investments
# Also selects the main sectors where investments are between 5 and 15 million
Avg_Raised_Amt <- function(x)
{
  country_main_sector <- summarise(x, mean(raised_amount_usd), n())
  colnames(country_main_sector) <- c("main_sector","avg_raised_amt_usd","no. of investments")
  country_main_sector <- subset(country_main_sector, avg_raised_amt_usd > 5000000 & avg_raised_amt_usd < 15000000)
  return(country_main_sector)
}

# calling the avg_raised_amt function 
D1_india_main_sector <- Avg_Raised_Amt(D1_india_invest_grp)
D2_usa_main_sector <- Avg_Raised_Amt(D2_usa_invest_grp)
D3_gbr_main_sector <- Avg_Raised_Amt(D3_gbr_invest_grp)

?summarise

# Summarises the main sectors with sum raised amount, number of investments
# Also selects the main sectors where investments are between 5 and 15 million
Sum_Raised_Amt <- function(x)
{
  country_main_sector <- summarise(x, sum(raised_amount_usd), n())
  colnames(country_main_sector) <- c("main_sector","sum_raised_amt_usd","no. of investments")
  #country_main_sector <- subset(country_main_sector, sum_raised_amt_usd > 5000000 & sum_raised_amt_usd < 15000000)
  return(country_main_sector)
}

# calling the sum_raised_amt function 
D1_india_main_sector_sum <- Sum_Raised_Amt(D1_india_invest_grp)
D2_usa_main_sector_sum <- Sum_Raised_Amt(D2_usa_invest_grp)
D3_gbr_main_sector_sum <- Sum_Raised_Amt(D3_gbr_invest_grp)

#Which company got highest investment where sector have highest no. of investment
?filter
?max
?tostring

Ind_comp<-filter(D1_india_investment,main_sector=='Others' & raised_amount_usd==max(raised_amount_usd))$name
###############HERE_TO
Usa_comp<-filter(D2_usa_investment,main_sector=='Others' & raised_amount_usd==max(raised_amount_usd))$name
Gbr_comp<-filter(D3_gbr_investment,main_sector=='Others' & raised_amount_usd==max(raised_amount_usd))$name

Ind_comp
Usa_comp
Gbr_comp

Ind_comp2<-filter(D1_india_investment,trim(main_sector)=='News..Search.and.Messaging' & raised_amount_usd==max(raised_amount_usd))$name
Usa_comp2<-filter(D2_usa_investment,main_sector=='Social..Finance..Analytics..Advertising' & raised_amount_usd==max(raised_amount_usd))$name
Gbr_comp2<-filter(D3_gbr_investment,main_sector=='Cleantech...Semiconductors' & raised_amount_usd==max(raised_amount_usd))$name

Ind_comp2

D1_india_investment[which(D1_india_investment$main_sector=='News..Search.and.Messaging' & D1_india_investment$raised_amount_usd==max(D1_india_investment$raised_amount_usd)),'name']
#################HERE not working please check
# final output
write.csv(Final_Master_P,"final_master.csv")
