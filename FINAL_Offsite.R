library(caret)
library(dplyr)
library(rio)
library(taRifx)

GC <- "All visitors_Aug 24 - 31 2020.xlsx"
adata_list <- import_list(GC)
off_page_surge_File <- adata_list$`Offsite surge`
dim(off_page_surge_File)
write.csv(off_page_surge_File,'off_page_surge_File.csv')
off_page_surge_File <- read.csv("off_page_surge_File.csv", header = T)
off_page_surge_File <- off_page_surge_File[-c(1)]
dim(off_page_surge_File)
#View(off_page_surge_File$Account..Demandbase.Last.Updated)
#opening overview and adding columns to offpage
overview_file <- adata_list$Overview
dim(overview_file)
write.csv(overview_file,'dummy.csv')
overview_file <- read.csv("dummy.csv", header = T)
overview_file <- overview_file[-c(1)]
dim(overview_file)
off_page_surge_File[is.na(off_page_surge_File)] = 0
#View(off_page_surge_File)
#write.csv(off_page_surge_File,'off_page_surge_File.csv')
#off_page_surge_File$session_duration_ <- overview_file$Avg..Session.Duration[match(off_page_surge_File$Account.Name,overview_file$Company.Name)]
off_page_surge_File$session_duration_1 <- overview_file$Avg..Session.Duration[match(off_page_surge_File$Account.Domain,overview_file$Web.Site)]
#off_page_surge_File$session_duration_2 <- overview_file$Avg..Session.Duration[match(off_page_surge_File$Account.Name,overview_file$Company.Name)]
off_page_surge_File[is.na(off_page_surge_File)] = 0
str(off_page_surge_File$session_duration_1)
#str(off_page_surge_File$session_duration_2)
#View(off_page_surge_File$session_duration_2)
off_page_surge_File[is.na(off_page_surge_File)] = 0
str(off_page_surge_File$Avg.Session.Duration)
off_page_surge_File$session_duration <- (off_page_surge_File$session_duration_1 + off_page_surge_File$Avg.Session.Duration)
str(off_page_surge_File$session_duration)
#off_page_surge_File$session_duration <- (off_page_surge_File$session_duration_1 + off_page_surge_File$session_duration_2 + off_page_surge_File$Avg.Session.Duration)
off_page_surge_File[is.na(off_page_surge_File)] = 0
#off_page_surge_File$Page_views_1 <- overview_file$Pageviews[match(off_page_surge_File$Account.Name,overview_file$Company.Name)]


off_page_surge_File$Page_views <- overview_file$Pageviews[match(off_page_surge_File$Account.Domain,overview_file$Web.Site)]
off_page_surge_File$Bounce_Rate <- overview_file$Bounce.Rate[match(off_page_surge_File$Account.Domain,overview_file$Web.Site)]
off_page_surge_File[is.na(off_page_surge_File)] = 0


#Removing unnecessaru columns
drop <- c('Avg.Session.Duration','Bounce.Rate','Pageviews','session_duration_2','session_duration_1',
          'Bounce_Rate_1','Bounce_Rate_2','Page_views_2','Page_views_1',
          'Account.Managers','Vertical','SID','Account.Rank','Hexaware.Or.Mobiquity',
          'NAICS.code','SIC.Code','Zip','AI.ranked.this.by','Hq.State','Hq.City','Heatmap','Date',
          'Services.Engaged','Secondary.Industries','Primary.Industry','Oracle.Focus',
          'Website.Url','Geo','Revenue','Account.ID','Top.10.Accounts',
          'Revenue.Range...46','Industry...37','Employee.Range...36')
off_page_surge_File = off_page_surge_File[,!(names(off_page_surge_File) %in% drop)]
dim(off_page_surge_File)
str(off_page_surge_File$session_duration)
#write.csv(off_page_surge_File,'off_page_surge_File.csv')

#View(off_page_surge_File)
#Renaming overall score
colnames(off_page_surge_File)[colnames(off_page_surge_File) == 'Overall.Score.Number..6.decimals.'] <- 'overall_score'
colnames(off_page_surge_File)[colnames(off_page_surge_File) == 'Overall.Score.Number..3.decimals.'] <- 'overall_score'
colnames(off_page_surge_File)[colnames(off_page_surge_File) == 'Industry...8'] <- 'Industry'
colnames(off_page_surge_File)[colnames(off_page_surge_File) == 'Revenue.Range...9'] <- 'Revenue.Range'
colnames(off_page_surge_File)[colnames(off_page_surge_File) == 'Employee.Range...10'] <- 'Employee.Range'

#view(off_page_surge_File)
# Creating visit's Source of origin 
off_page_surge_File$Source <- "Offsite Intent"
off_page_surge_File$Source <- as.factor(off_page_surge_File$Source)



#Structuring the Column types
off_page_surge_File$Account.Name <- as.factor(off_page_surge_File$Account.Name)
off_page_surge_File$Account.Domain <- as.factor(off_page_surge_File$Account.Domain)
off_page_surge_File$overall_score <- as.numeric(off_page_surge_File$overall_score)
off_page_surge_File$Account..Demandbase.Qualification.Score <- as.factor(off_page_surge_File$Account..Demandbase.Qualification.Score)
off_page_surge_File$Industry <- as.factor(off_page_surge_File$Industry)
off_page_surge_File$Revenue.Range <- as.factor(off_page_surge_File$Revenue.Range)
off_page_surge_File$Employee.Range <- as.factor(off_page_surge_File$Employee.Range)
off_page_surge_File$City <- as.factor(off_page_surge_File$City)
off_page_surge_File$State <- as.factor(off_page_surge_File$State)
off_page_surge_File$Country <- as.factor(off_page_surge_File$Country)
off_page_surge_File$Specializes.in <- as.factor(off_page_surge_File$Specializes.in)
off_page_surge_File$Active.Buyer.Roles <- as.factor(off_page_surge_File$Active.Buyer.Roles)
off_page_surge_File$Account..Demandbase.Page.Views.Last.30.Days <- as.numeric(off_page_surge_File$Account..Demandbase.Page.Views.Last.30.Days)
off_page_surge_File$Days.Ago..last.seen. <- as.numeric(off_page_surge_File$Days.Ago..last.seen.)
off_page_surge_File$Percent.Increase.Traffic.MoM <- gsub("%", "", off_page_surge_File$Percent.Increase.Traffic.MoM)
off_page_surge_File$Percent.Increase.Traffic.MoM <- as.numeric(off_page_surge_File$Percent.Increase.Traffic.MoM)
is.na(off_page_surge_File$Percent.Increase.Traffic.MoM) <- 0
#off_page_surge_File$Websites..Top.5. <- as.factor(off_page_surge_File$Websites..Top.5.)
#off_page_surge_File$Existing.Customer <- as.factor(off_page_surge_File$Existing.Customer)
off_page_surge_File$Intent..Top.5..account.level.intent. <- as.factor(off_page_surge_File$Intent..Top.5..account.level.intent.)
off_page_surge_File$session_duration <- as.numeric(off_page_surge_File$session_duration)
#off_page_surge_File$Hexaware.Or.Mobiquity <- as.factor(off_page_surge_File$Hexaware.Or.Mobiquity)
off_page_surge_File$Bounce_Rate <- as.numeric(off_page_surge_File$Bounce_Rate)
#off_page_surge_File$Account.Managers <- as.factor(off_page_surge_File$Account.Managers)
#off_page_surge_File$Vertical <- as.factor(off_page_surge_File$Vertical)
off_page_surge_File$Sessions <- as.numeric(off_page_surge_File$Sessions)
#off_page_surge_File$Top.10.Accounts <- as.factor(off_page_surge_File$Top.10.Accounts)
#off_page_surge_File$Bounce_Rate <- as.numeric(off_page_surge_File$Bounce_Rate)
dim(off_page_surge_File)
str(off_page_surge_File)


#Details
offpage_surge_account_details <- off_page_surge_File[c('Account.Name','Account.Domain',
                                                       'overall_score','Account..Demandbase.Qualification.Score','Industry','Revenue.Range',
                                                       'Employee.Range','City','State','Country','Specializes.in','Active.Buyer.Roles',
                                                       'Account..Demandbase.Last.Updated','Account..Demandbase.Page.Views.Last.30.Days','Days.Ago..last.seen.',
                                                       'Websites..Top.5.','Intent..Top.5..account.level.intent.',
                                                       'Existing.Customer','session_duration',
                                                       'X..of.Employees','Percent.Increase.Traffic.MoM',
                                                       'Page_views','Sessions','Bounce_Rate','Source')]

#offpage_surge_account_details[is.na(offpage_surge_account_details)] <- 0
dim(offpage_surge_account_details)
#Info
drop <- c('Account.Name','Users','It.Budget.Mil','session_duration',
          'overall_score','Account..Demandbase.Qualification.Score','Industry','Revenue.Range',
          'Employee.Range','City','State','Country','Specializes.in','Active.Buyer.Roles',
          'Account..Demandbase.Last.Updated','Account..Demandbase.Page.Views.Last.30.Days','Days.Ago..last.seen.',
          'Websites..Top.5.','Intent..Top.5..account.level.intent.',
          'Existing.Customer','Bounce_Rate',
          'X..of.Employees','Percent.Increase.Traffic.MoM',
          'Page_views','Sessions','Top.10.Accounts')
offpage_surge_account_info = off_page_surge_File[,!(names(off_page_surge_File) %in% drop)]
dim(offpage_surge_account_info)
offpage_surge_account_info <- japply( offpage_surge_account_info, which(sapply(offpage_surge_account_info, class)=="logical"), as.numeric )
#str(offpage_surge_account_info)
#offpage_surge_account_info$Account.Name <- as.character(offpage_surge_account_info$Account.Name)
#str(offpage_surge_account_info)
#View(offpage_surge_account_info)
offpage_surge_account_info[is.na(offpage_surge_account_info)] <- 0
#new_data <- left_join(offpage_surge_account_details, offpage_surge_account_info, by = "Account.Domain")



write.csv(offpage_surge_account_details,'DUMP_offsite_DETAILS.csv')
write.csv(offpage_surge_account_info,'DUMP_offsite_INFO.csv')
dim(offpage_surge_account_details)
dim(offpage_surge_account_info)



##############################

library(caret)
library(dplyr)
library(rio)
VJ<- c("All visitors_Jan 6-12 2020.xlsx","All visitors_Jan 13 - Feb 2 2020.xlsx",
       "All visitors_Feb 3-9 2020.xlsx","All visitors_Feb 17-23 2020.xlsx",
       "All visitors_Feb 24-March 1 2020.xlsx",
       "All visitors_March 2-15 2020.xlsx",
       "All visitors_March 16-22 2020.xlsx","All visitors_March 23-29 2020.xlsx",
       "All visitors_March 30 - April 5 2020.xlsx","All visitors_April 6-12 2020.xlsx",
       "All visitors_April 13-19 2020.xlsx",
       
       
       
       "All visitors_June 1-14 2020.xlsx","All visitors_June 15-21 2020.xlsx",
       "All visitors_June 22 - July 5 2020.xlsx","All visitors_July 6-12 2020.xlsx",
       "All visitors_July 13-19 2020.xlsx","All visitors_July 20-26 2020.xlsx",
       "All visitors_July 27 - Aug 2 2020.xlsx", "All visitors_Aug 3-9 2020.xlsx",
       "All visitors_Aug 10-16 2020.xlsx", "All visitors_Aug 17-23 2020.xlsx",
       "All visitors_Aug 24-25 2020.xlsx"
)


for (GC in VJ) {
  
  print(GC)
  adata_list <- import_list(GC)
  off_page_surge_File <- adata_list$`Offsite surge`
  dim(off_page_surge_File)
  write.csv(off_page_surge_File,'off_page_surge_File.csv')
  off_page_surge_File <- read.csv("off_page_surge_File.csv", header = T)
  off_page_surge_File <- off_page_surge_File[-c(1)]
  dim(off_page_surge_File)
  #View(off_page_surge_File$Account..Demandbase.Last.Updated)
  #opening overview and adding columns to offpage
  overview_file <- adata_list$Overview
  dim(overview_file)
  write.csv(overview_file,'dummy.csv')
  overview_file <- read.csv("dummy.csv", header = T)
  overview_file <- overview_file[-c(1)]
  dim(overview_file)
  off_page_surge_File[is.na(off_page_surge_File)] = 0
  #View(off_page_surge_File)
  #write.csv(off_page_surge_File,'off_page_surge_File.csv')
  #off_page_surge_File$session_duration_ <- overview_file$Avg..Session.Duration[match(off_page_surge_File$Account.Name,overview_file$Company.Name)]
  off_page_surge_File$session_duration_1 <- overview_file$Avg..Session.Duration[match(off_page_surge_File$Account.Domain,overview_file$Web.Site)]
  #off_page_surge_File$session_duration_2 <- overview_file$Avg..Session.Duration[match(off_page_surge_File$Account.Name,overview_file$Company.Name)]
  off_page_surge_File[is.na(off_page_surge_File)] = 0
  str(off_page_surge_File$session_duration_1)
  str(off_page_surge_File$session_duration_2)
  #View(off_page_surge_File$session_duration_2)
  off_page_surge_File[is.na(off_page_surge_File)] = 0
  str(off_page_surge_File$Avg.Session.Duration)
  off_page_surge_File$session_duration <- (off_page_surge_File$session_duration_1 + off_page_surge_File$Avg.Session.Duration)
  str(off_page_surge_File$session_duration)
  #off_page_surge_File$session_duration <- (off_page_surge_File$session_duration_1 + off_page_surge_File$session_duration_2 + off_page_surge_File$Avg.Session.Duration)
  off_page_surge_File[is.na(off_page_surge_File)] = 0

  off_page_surge_File$Page_views <- overview_file$Pageviews[match(off_page_surge_File$Account.Domain,overview_file$Web.Site)]
  off_page_surge_File$Bounce_Rate <- overview_file$Bounce.Rate[match(off_page_surge_File$Account.Domain,overview_file$Web.Site)]
  off_page_surge_File[is.na(off_page_surge_File)] = 0
  
    #Removing unnecessaru columns
  drop <- c('Avg.Session.Duration','Bounce.Rate','Pageviews','session_duration_2','session_duration_1',
            'Bounce_Rate_1','Bounce_Rate_2','Page_views_2','Page_views_1',
            'Account.Managers','Vertical','SID','Account.Rank','Hexaware.Or.Mobiquity',
            'NAICS.code','SIC.Code','Zip','AI.ranked.this.by','Hq.State','Hq.City','Heatmap','Date',
            'Services.Engaged','Secondary.Industries','Primary.Industry','Oracle.Focus',
            'Website.Url','Geo','Revenue','Account.ID','Top.10.Accounts',
            'Revenue.Range...46','Industry...37','Employee.Range...36')
  off_page_surge_File = off_page_surge_File[,!(names(off_page_surge_File) %in% drop)]
  dim(off_page_surge_File)
  str(off_page_surge_File$session_duration)
  #write.csv(off_page_surge_File,'off_page_surge_File.csv')
  
  #View(off_page_surge_File)
  #Renaming overall score
  colnames(off_page_surge_File)[colnames(off_page_surge_File) == 'Overall.Score.Number..6.decimals.'] <- 'overall_score'
  colnames(off_page_surge_File)[colnames(off_page_surge_File) == 'Overall.Score.Number..3.decimals.'] <- 'overall_score'
  colnames(off_page_surge_File)[colnames(off_page_surge_File) == 'Industry...8'] <- 'Industry'
  colnames(off_page_surge_File)[colnames(off_page_surge_File) == 'Revenue.Range...9'] <- 'Revenue.Range'
  colnames(off_page_surge_File)[colnames(off_page_surge_File) == 'Employee.Range...10'] <- 'Employee.Range'
  
  #view(off_page_surge_File)
  # Creating visit's Source of origin 
  off_page_surge_File$Source <- "Offsite Intent"
  off_page_surge_File$Source <- as.factor(off_page_surge_File$Source)
  
  
  
  #Structuring the Column types
  off_page_surge_File$Account.Name <- as.factor(off_page_surge_File$Account.Name)
  off_page_surge_File$Account.Domain <- as.factor(off_page_surge_File$Account.Domain)
  off_page_surge_File$overall_score <- as.numeric(off_page_surge_File$overall_score)
  off_page_surge_File$Account..Demandbase.Qualification.Score <- as.factor(off_page_surge_File$Account..Demandbase.Qualification.Score)
  off_page_surge_File$Industry <- as.factor(off_page_surge_File$Industry)
  off_page_surge_File$Revenue.Range <- as.factor(off_page_surge_File$Revenue.Range)
  off_page_surge_File$Employee.Range <- as.factor(off_page_surge_File$Employee.Range)
  off_page_surge_File$City <- as.factor(off_page_surge_File$City)
  off_page_surge_File$State <- as.factor(off_page_surge_File$State)
  off_page_surge_File$Country <- as.factor(off_page_surge_File$Country)
  off_page_surge_File$Specializes.in <- as.factor(off_page_surge_File$Specializes.in)
  off_page_surge_File$Active.Buyer.Roles <- as.factor(off_page_surge_File$Active.Buyer.Roles)
  off_page_surge_File$Account..Demandbase.Page.Views.Last.30.Days <- as.numeric(off_page_surge_File$Account..Demandbase.Page.Views.Last.30.Days)
  off_page_surge_File$Days.Ago..last.seen. <- as.numeric(off_page_surge_File$Days.Ago..last.seen.)
  off_page_surge_File$Percent.Increase.Traffic.MoM <- gsub("%", "", off_page_surge_File$Percent.Increase.Traffic.MoM)
  off_page_surge_File$Percent.Increase.Traffic.MoM <- as.numeric(off_page_surge_File$Percent.Increase.Traffic.MoM)
  is.na(off_page_surge_File$Percent.Increase.Traffic.MoM) <- 0
  #off_page_surge_File$Websites..Top.5. <- as.factor(off_page_surge_File$Websites..Top.5.)
  #off_page_surge_File$Existing.Customer <- as.factor(off_page_surge_File$Existing.Customer)
  off_page_surge_File$Intent..Top.5..account.level.intent. <- as.factor(off_page_surge_File$Intent..Top.5..account.level.intent.)
  off_page_surge_File$session_duration <- as.numeric(off_page_surge_File$session_duration)
  #off_page_surge_File$Hexaware.Or.Mobiquity <- as.factor(off_page_surge_File$Hexaware.Or.Mobiquity)
  off_page_surge_File$Bounce_Rate <- as.numeric(off_page_surge_File$Bounce_Rate)
  #off_page_surge_File$Account.Managers <- as.factor(off_page_surge_File$Account.Managers)
  #off_page_surge_File$Vertical <- as.factor(off_page_surge_File$Vertical)
  off_page_surge_File$Sessions <- as.numeric(off_page_surge_File$Sessions)
  #off_page_surge_File$Top.10.Accounts <- as.factor(off_page_surge_File$Top.10.Accounts)
  #off_page_surge_File$Bounce_Rate <- as.numeric(off_page_surge_File$Bounce_Rate)
  dim(off_page_surge_File)
  str(off_page_surge_File)
  
  
  #Details
  offpage_surge_account_details <- off_page_surge_File[c('Account.Name','Account.Domain',
                                                         'overall_score','Account..Demandbase.Qualification.Score','Industry','Revenue.Range',
                                                         'Employee.Range','City','State','Country','Specializes.in','Active.Buyer.Roles',
                                                         'Account..Demandbase.Last.Updated','Account..Demandbase.Page.Views.Last.30.Days','Days.Ago..last.seen.',
                                                         'Websites..Top.5.','Intent..Top.5..account.level.intent.',
                                                         'Existing.Customer','session_duration',
                                                         'X..of.Employees','Percent.Increase.Traffic.MoM',
                                                         'Page_views','Sessions','Bounce_Rate','Source')]
  
  #offpage_surge_account_details[is.na(offpage_surge_account_details)] <- 0
  dim(offpage_surge_account_details)
  #Info
  drop <- c('Account.Name','Users','It.Budget.Mil','session_duration',
            'overall_score','Account..Demandbase.Qualification.Score','Industry','Revenue.Range',
            'Employee.Range','City','State','Country','Specializes.in','Active.Buyer.Roles',
            'Account..Demandbase.Last.Updated','Account..Demandbase.Page.Views.Last.30.Days','Days.Ago..last.seen.',
            'Websites..Top.5.','Intent..Top.5..account.level.intent.',
            'Existing.Customer','Bounce_Rate',
            'X..of.Employees','Percent.Increase.Traffic.MoM',
            'Page_views','Sessions','Top.10.Accounts')
  offpage_surge_account_info = off_page_surge_File[,!(names(off_page_surge_File) %in% drop)]
  dim(offpage_surge_account_info)
  offpage_surge_account_info <- japply( offpage_surge_account_info, which(sapply(offpage_surge_account_info, class)=="logical"), as.numeric )
  #str(offpage_surge_account_info)
  #offpage_surge_account_info$Account.Name <- as.character(offpage_surge_account_info$Account.Name)
  #str(offpage_surge_account_info)
  #View(offpage_surge_account_info)
  offpage_surge_account_info[is.na(offpage_surge_account_info)] <- 0
  #new_data <- left_join(offpage_surge_account_details, offpage_surge_account_info, by = "Account.Domain")
  
  
  write.csv(offpage_surge_account_details,'dummy_details.csv')
  write.csv(offpage_surge_account_info,'dummy_info.csv')
  dim(offpage_surge_account_details)
  dim(offpage_surge_account_info)
  
  
  
  ##################################################################
  
  #Appending DUMP and Newdata for DETAILS
  dump_data_details <- read.csv("DUMP_offsite_DETAILS.csv", header = T)
  dump_data_details <- dump_data_details[-c(1)]
  new_data <- read.csv("dummy_details.csv", header = T)
  new_data <- new_data[-c(1)]
  #view(past_data)
  dim(new_data)
  dim(dump_data_details)
  
  new_details <- full_join(new_data, dump_data_details)
  dim(new_details)
  #new[is.na(new_details)] <- 0
  write.csv(new_details, "DUMP_offsite_DETAILS.csv")
  dim(new_details)
  #warnings()
  
  #Appending DUMP and Newdata for INFO
  dump_data_info <- read.csv("DUMP_offsite_INFO.csv", header = T)
  dump_data_info <- dump_data_info[-c(1)]
  new_data <- read.csv("dummy_info.csv", header = T)
  new_data <- new_data[-c(1)]
  
  #view(past_data)
  
  dim(new_data)
  dim(dump_data_info)
  
  new_info <- full_join(new_data, dump_data_info)
  dim(new_info)
  #new[is.na(new_info)] <- 0
  write.csv(new_info, "DUMP_offsite_INFO.csv")
  dim(new_info)
  #warnings()
  
  
  
}


#For Final Verification
details <- read.csv("DUMP_offsite_DETAILS.csv")
details <- details[-c(1)]
info <- read.csv("DUMP_offsite_INFO.csv")
info <- info[-c(1)]
dim(details)
dim(info)



