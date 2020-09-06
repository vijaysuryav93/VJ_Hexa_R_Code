#Appending All DETAILS and INFO FILE

# DETAILS

overall_details <- read.csv("DUMP_overall_DETAILS.csv")
overall_details <- overall_details[-c(1)]
offpage_details <- read.csv("DUMP_offpage_Details_Dump.csv")
offpage_details <- offpage_details[-c(1)]
offsite_details <- read.csv("DUMP_offsite_DETAILS.csv")
offsite_details <- offsite_details[-c(1)]
onsite_details <- read.csv("DUMP_onsite_DETAILS.csv")
onsite_details <- onsite_details[-c(1)]


#INFO

overall_info <- read.csv("DUMP_overall_INFO.csv")
overall_info <- overall_info[-c(1)]
offpage_info <- read.csv("DUMP_offpage_INFO_Dump.csv")
offpage_info <- offpage_info[-c(1)]
offsite_info <- read.csv("DUMP_offsite_INFO.csv")
offsite_info <- offsite_info[-c(1)]
onsite_info <- read.csv("DUMP_onsite_INFO.csv")
onsite_info <- onsite_info[-c(1)]

#View(offpage_info)


#Verify dimensions
dim(overall_details)
dim(overall_info)
dim(offpage_details)
dim(offpage_info)
dim(offsite_details)
dim(offsite_info)
dim(onsite_details)
dim(onsite_info)



#Appending DETAILS
details_1 <- rbind(onsite_details,offsite_details)
details_2 <- rbind(overall_details,offpage_details)
details <- rbind(details_1,details_2)
dim(details)
#Remooving words in web URL
new <- details


new$Account..Demandbase.Qualification.Score <- gsub("high", "High", new$Account..Demandbase.Qualification.Score)
new$Account..Demandbase.Qualification.Score <- gsub("HIGH", "High", new$Account..Demandbase.Qualification.Score)
new$Account..Demandbase.Qualification.Score <- gsub("low", "Low", new$Account..Demandbase.Qualification.Score)
new$Account..Demandbase.Qualification.Score <- gsub("LOW", "Low", new$Account..Demandbase.Qualification.Score)
new$Account..Demandbase.Qualification.Score <- gsub("medium", "Medium", new$Account..Demandbase.Qualification.Score)
new$Account..Demandbase.Qualification.Score <- gsub("MEDIUM", "Medium", new$Account..Demandbase.Qualification.Score)
new$Websites..Top.5. <- gsub("hexaware.com/,", "", new$Websites..Top.5.)
new$Websites..Top.5. <- gsub("hexaware.com/", "", new$Websites..Top.5.)
new$Websites..Top.5. <- gsub("hexaware.com,", "", new$Websites..Top.5.)
new$Websites..Top.5. <- gsub("blogs/,", "", new$Websites..Top.5.)
new$Websites..Top.5. <- gsub("services/,", "", new$Websites..Top.5.)
new$Websites..Top.5. <- gsub("resource/,", "", new$Websites..Top.5.)
new$Websites..Top.5. <- gsub("event/,", "", new$Websites..Top.5.)
new$Websites..Top.5. <- gsub("application-transformation-management/,", "", new$Websites..Top.5.)
new$Websites..Top.5. <- gsub(" industries/,", "", new$Websites..Top.5.)
new$Websites..Top.5. <- gsub(",", "  ;  ", new$Websites..Top.5.)
new$Websites..Top.5. <- gsub(" ;     ;", "  ;  ", new$Websites..Top.5.)
new$Websites..Top.5. <- gsub("  ;   /", "  ; ", new$Websites..Top.5.)
new$Websi
dim(new)
new[is.na(new)] = 0


#Appending TAL List to Details by matching Account Domain
data_list <- read.csv("MASTER_TAL_and_EN_MATCH_VisitorType_Lookup.csv", header = T)
dim(data_list)
new$CustomerType <- data_list$CustomerType[match(new$Account.Domain,data_list$AccountDomain)]
write.csv(new, "DETAILS.csv")
dim(new)


#Appending INFO
library(dplyr)

#info_1 <- full_join(onsite_info,offsite_info)
#info_2 <- full_join(overall_info,offpage_info)
#info <- full_join(info_1,info_2)
#dim(info)
#info[is.na(info)] <- 0
#write.csv(info,"FINAL_INFO.csv")
#dim(info)

#Total <- merge(details,info,by="Account.Domain")
#dim(Total)
#write.csv(Total,"FINAL_Total.csv")


#Keyword FIle operation
library(caret)
library(dplyr)
library(rio)
library(taRifx)


info <- read.csv("DUMP_offpage_INFO_Dump.csv")
dim(info)
#info <- info[-c(1)]
#View(info)
#info$Acocunt.Domain
drop <- c('Acocunt.Domain','Acccount.Domain','Number.of.Buyers','Revenue.Range...37','Country.Name','Party.Name',
          'Asset.Conversions.Goal.9.Completions','Website.Domain','Account.Domains','Hq.Phone','Account.Id',
          'Acount.Name','Account.Name.Provided.By.Sales','Sub.Industry','Page','Web.Site','Industry...51',
          'Employee.Range...52','Industry...39','Revenue.Range...48','Source','Employee.Range...31'
          
)
info = info[,!(names(info) %in% drop)]
dim(info)
info[is.na(info)] <- 0
write.csv(info, "LOOKUP_keyword.csv")
dim(info)

