library(caret)
library(dplyr)
library(rio)
library(taRifx)
library("qdapRegex")

data_list <- read.csv("YTD_ALL.csv", header = T)
dim(data_list)

overview <- read.csv("overview_Aug_29.csv", header = T)
dim(overview)


#Copying Values from overview (Google Analytics) to Demandbase
data_list$session_duration_1 <- overview$Avg..Session.Duration[match(data_list$Account.Domain,overview$Web.Site)]
data_list[is.na(data_list)] = 0
str(data_list$session_duration_1)
data_list$session_duration <- (data_list$session_duration_1 + data_list$Avg.Session.Duration)
str(data_list$session_duration)


data_list$Page_views <- overview$Pageviews[match(data_list$Account.Domain,overview$Web.Site)]
data_list$Bounce_Rate <- overview$Bounce.Rate[match(data_list$Account.Domain,overview$Web.Site)]
data_list$Bounce_Rate <- gsub("%", "", data_list$Bounce_Rate)
data_list$Bounce_Rate <- as.numeric(data_list$Bounce_Rate)
data_list$Bounce_Rate <- data_list$Bounce_Rate/100
data_list$Sessions <- overview$Sessions[match(data_list$Account.Domain,overview$Web.Site)]
data_list$Users_Count <- overview$Users[match(data_list$Account.Domain,overview$Web.Site)]
data_list[is.na(data_list)] = 0


#Removing unnecessaru columns
drop <- c('Pageviews','Bounce.Rate','Users',
  'session_duration_2','session_duration_1', 'It.Budget.Mil',
            'Bounce_Rate_1','Bounce_Rate_2','Page_views_2','Page_views_1',
            'Account.Managers','Vertical','SID','Hexaware.Or.Mobiquity',
            'NAICS.code','SIC.Code','Zip','AI.ranked.this.by','Hq.State','Hq.City','Heatmap','Date',
            'Services.Engaged','Secondary.Industries','Primary.Industry','Oracle.Focus',
            'Website.Url','Geo','Revenue','Account.ID','Top.10.Accounts',
            'Revenue.Range...46','Industry...37','Employee.Range...36')
data_list = data_list[,!(names(data_list) %in% drop)]
dim(data_list)
surge_file <- data_list
dim(surge_file)
#Renaming overall score
colnames(surge_file)[colnames(surge_file) == 'Overall.Score.Number..6.decimals.'] <- 'overall_score'
colnames(surge_file)[colnames(surge_file) == 'Overall.Score.Number..3.decimals.'] <- 'overall_score'
colnames(surge_file)[colnames(surge_file) == 'Industry...8'] <- 'Industry'
colnames(surge_file)[colnames(surge_file) == 'Revenue.Range...9'] <- 'Revenue.Range'
colnames(surge_file)[colnames(surge_file) == 'Employee.Range...10'] <- 'Employee.Range'

# Creating visit's Source of origin 
surge_file$Source <- "ML Prediction"
surge_file$Source <- as.factor(surge_file$Source)


#Structuring the Column types
surge_file$Account.Rank <- as.factor(surge_file$Account.Rank)
surge_file$Account.Name <- as.factor(surge_file$Account.Name)
surge_file$Account.Domain <- as.factor(surge_file$Account.Domain)
surge_file$Account..Demandbase.Qualification.Score <- as.factor(surge_file$Account..Demandbase.Qualification.Score)
surge_file$Industry <- as.factor(surge_file$Industry)
surge_file$Revenue.Range <- as.factor(surge_file$Revenue.Range)
surge_file$Employee.Range <- as.factor(surge_file$Employee.Range)
surge_file$City <- as.factor(surge_file$City)
surge_file$State <- as.factor(surge_file$State)
surge_file$Country <- as.factor(surge_file$Country)
surge_file$Specializes.in <- as.factor(surge_file$Specializes.in)
surge_file$Active.Buyer.Roles <- as.factor(surge_file$Active.Buyer.Roles)
surge_file$Intent..Top.5..account.level.intent. <- as.factor(surge_file$Intent..Top.5..account.level.intent.)

#Important Variables for classification
surge_file$overall_score <- as.numeric(surge_file$overall_score)
surge_file$Avg.Session.Duration <- as.numeric(surge_file$Avg.Session.Duration)
surge_file$Bounce_Rate <- as.numeric(surge_file$Bounce_Rate)
surge_file$Sessions <- as.numeric(surge_file$Sessions)
surge_file$Page_views <- as.numeric(surge_file$Page_views)
surge_file$Days.Ago..last.seen. <- as.numeric(surge_file$Days.Ago..last.seen.)
surge_file$Account..Demandbase.Page.Views.Last.30.Days <- as.numeric(surge_file$Account..Demandbase.Page.Views.Last.30.Days)
surge_file$Users_Count <- as.numeric(surge_file$Users_Count)
surge_file$Percent.Increase.Traffic.MoM <- gsub("%", "", surge_file$Percent.Increase.Traffic.MoM)
surge_file$Percent.Increase.Traffic.MoM <- as.numeric(surge_file$Percent.Increase.Traffic.MoM)
is.na(surge_file$Percent.Increase.Traffic.MoM) <- 0
dim(surge_file)
#str(surge_file)


#Replacing NA to zero
surge_file[is.na(surge_file)] = 0


#Creating Mean for all above important variables 
Length_Bounce_Rate <-length(which(surge_file$Bounce_Rate != 0))
Mean_Bounce_Rate <- sum(surge_file$Bounce_Rate, na.rm = TRUE) 
Mean_Bounce_Rate <- Mean_Bounce_Rate/Length_Bounce_Rate
Mean_Bounce_Rate

length_pageview <- length(which(surge_file$Page_views != 0))
Mean_Page_views <- sum(surge_file$Page_views, na.rm = TRUE) 
Mean_Page_views <- Mean_Page_views/length_pageview
Mean_Page_views

length_session_duration <- length(which(surge_file$Avg.Session.Duration != 0))
Mean_session_duration <- sum(surge_file$Avg.Session.Duration, na.rm = TRUE) 
Mean_session_duration <- Mean_session_duration/length_session_duration
Mean_session_duration

Length_overall_score <- length(which(surge_file$overall_score != 0))
Mean_overall_score <- sum(surge_file$overall_score, na.rm = TRUE) 
Mean_overall_score <- Mean_overall_score/Length_overall_score

Length_Sessions <- length(which(surge_file$Sessions != 0))
Mean_Sessions <- sum(surge_file$Sessions, na.rm = TRUE) 
Mean_Sessions <- Mean_Sessions/Length_Sessions

Length_users <- length(which(surge_file$Users_Count != 0))
Mean_users <- sum(surge_file$Users_Count, na.rm = TRUE) 
Mean_users <- Mean_users/Length_users

#Length_Days_ago_last_seen <- length(which(surge_file$Days.Ago..last.seen. != 0))
#surge_file$Days.Ago..last.seen. <- (1/surge_file$Days.Ago..last.seen.)
#Mean_Days_ago_last_seen <- sum(surge_file$Days.Ago..last.seen., na.rm = TRUE) 
#Mean_Days_ago_last_seen <- Mean_Days_ago_last_seen/Length_Days_ago_last_seen
#Mean_Days_ago_last_seen

#Length_Page_view_last_30days <- length(which(surge_file$Account..Demandbase.Page.Views.Last.30.Days. != 0))
#surge_file$Account..Demandbase.Page.Views.Last.30.Days <- (1/surge_file$Account..Demandbase.Page.Views.Last.30.Days)
#Mean_Page_view_last_30days <- mean(surge_file$Account..Demandbase.Page.Views.Last.30.Days, na.rm = TRUE) 
#Mean_Page_view_last_30days

#Length_percent_Traffic_increase_MoM <- length(which(surge_file$Percent.Increase.Traffic.MoM != 0))
#Mean_percent_Traffic_increase_MoM <- sum(surge_file$Percent.Increase.Traffic.MoM, na.rm = TRUE)
#Mean_percent_Traffic_increase_MoM <- Mean_percent_Traffic_increase_MoM/Length_percent_Traffic_increase_MoM


#Creating a derived Variable Hexa_Markup

surge_file$Hexa_Markup <- ifelse((surge_file$Bounce_Rate < Mean_Bounce_Rate) &
                                   (surge_file$Bounce_Rate != 1) &
                                   (surge_file$Page_views > Mean_Page_views) &
                                   (surge_file$Avg.Session.Duration > Mean_session_duration) &
                                   (surge_file$overall_score > Mean_overall_score) , 1, 0)
surge_file$Hexa_Markup <- as.factor(surge_file$Hexa_Markup)
#View(surge_file$Hexa_Markup)
write.csv(surge_file, "Test.csv")  


#Splitting into Details and info by using Account Rank as key


#Details

#Details
surge_file_details <- surge_file[c('Account.Name','Account.Domain','Account.Rank',
                                   'overall_score','Account..Demandbase.Qualification.Score','Industry','Revenue.Range',
                                   'Employee.Range','City','State','Country','Specializes.in','Active.Buyer.Roles',
                                   'Account..Demandbase.Last.Updated', 'Account..Demandbase.Page.Views.Last.30.Days','Days.Ago..last.seen.',
                                   'Websites..Top.5.','Intent..Top.5..account.level.intent.',
                                   'session_duration', 'Existing.Customer', 'X..of.Employees',
                                   'Percent.Increase.Traffic.MoM','Sessions',
                                   
                                   'Page_views','Bounce_Rate','Source','Hexa_Markup')]

dim(surge_file_details)
#View(surge_file_details$session_duration)
#names(surge_file_details)
#str(surge_file_details$Existing.Customer)
#Info
drop <- c('Account.Name','Account.Rank','Avg.Session.Duration',
          'Account..Demandbase.Qualification.Score','Industry','Revenue.Range',
          'Employee.Range','City','State','Country','Specializes.in','Active.Buyer.Roles',
          'Account..Demandbase.Last.Updated', 'Account..Demandbase.Page.Views.Last.30.Days',
          'Days.Ago..last.seen.',
          'Websites..Top.5.','Intent..Top.5..account.level.intent.',
          'Existing.Customer', 'X..of.Employees',
          'Percent.Increase.Traffic.MoM','Source')
surge_file_info = surge_file[,!(names(surge_file) %in% drop)]
dim(surge_file_info)
#View(surge_file_info)

#changing column into row name
surge_file_info <- data.frame(surge_file_info, row.names = 1)
str(surge_file_info)
#names(surge_file_info)

#Outlier treatment for info file
dim(surge_file_info)
dataforoutliertreatment<-surge_file_info[c(1:598)] #Removing Hexa_Markup variable since it is a factor
for (i in names(dataforoutliertreatment)){
  print(i)
  x <- dataforoutliertreatment[[paste(i)]]
  qnt <- quantile(x, probs=c(.01, .99), na.rm = T)
  caps <- quantile(x, probs=c(.01, .99), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  dataforoutliertreatment[[paste(i)]] <- x
}

#Removing columns with zero sum
dataforoutliertreatment <- dataforoutliertreatment[, apply(dataforoutliertreatment[1:(ncol(dataforoutliertreatment))], 2, sum)!=0] 
dim(dataforoutliertreatment)

#Adding Hexa_Markup var back again
dataforoutliertreatment$Hexa_Markup <- surge_file_info$Hexa_Markup
dim(dataforoutliertreatment)
surge_file_info <- dataforoutliertreatment

surge_file_info$Bounce_Rate <- (1 - surge_file_info$Bounce_Rate) #THis is done because while calculating, algoritm ll give high value for bounce rate 1. So we are taking reverse  by subtracting by 1
surge_file_info <- japply( surge_file_info, which(sapply(surge_file_info, class)=="logical"), as.numeric )
surge_file_info[is.na(surge_file_info)] <- 0
#surge_file_info$Account.Rank <- as.factor(surge_file_info$Account.Rank)
str(surge_file_info$Account.Rank)
str(surge_file_info)
#surge_file_info$Account.Rank <- as.numeric(surge_file_info$Account.Rank)
surge_file_info[is.na(surge_file_info)] = 0
write.csv(surge_file_details,"dummy_details.csv")
write.csv(surge_file_info,"dummy_info.csv")



###########################################################################

set.seed(2341)
trainindex <- createDataPartition(surge_file_info$Hexa_Markup, p=0.9, list = F)
train_df <- surge_file_info[trainindex,]
#test_df <- surge_file_info[-trainindex,]
test_df <- surge_file_info
nrow(test_df)
nrow(train_df)
str(train_df$Hexa_Markup)
lg_train_df <- as.data.frame(train_df)
#View(lg_train_df)
#View(offpage_surge_account_info)
lg_test_df <- as.data.frame(test_df)
#View(lg_test_df)
#write.csv(lg_train_df,'C:/Users/Admin/Desktop/Desktop/Till/VJ_PGDM/3rd_Sem/BA/Logistic Reg 10 Sep/sSurge_train.csv')




lg_full_model = glm(lg_train_df$Hexa_Markup ~., data = lg_train_df, family = binomial, maxit = 100)

summary(lg_full_model)
criteria <- predict(lg_full_model, newdata=lg_test_df, type='response')



## Prediction on Development Dataset WITH aLL VARIABLES 
library(ROCR)
lg_test_df$prediction=predict(lg_full_model, newdata = lg_test_df,type = "response")
ROCRpred = prediction(lg_test_df$prediction, lg_test_df$Hexa_Markup)
threshold = as.numeric(performance(ROCRpred, "auc")@y.values)
threshold
perf = performance(ROCRpred, "tpr","fpr")
plot(perf)
plot(perf, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7))


#View(lg_test_df)
#lg_test_df[criteria<0.5, "criteria"] <- "Not Eligible"
#lg_test_df[criteria>=0.5, "criteria"] <- "Eligible"
lg_test_df[criteria<threshold, "criteria"] <- "Not"
lg_test_df[criteria>=threshold, "criteria"] <- "Target"
#View(lg_test_df)
#str(lg_test_df)
#dim(lg_test_df)
#lg_data <- lg_test_df[,c('Account.Rank','criteria')]
#names(lg_data)
#table(lg_test_df$criteria)
#confusionMatrix(table(lg_test_df[,20], lg_test_df[,21]))
#confusionMatrix(table(lg_test_df$status, lg_test_df$criteria))
#nrow(lg_data)
#View(lg_test_df$`Account Rank`)
write.csv(lg_test_df,'dummy.csv')
lg_data <- read.csv('dummy.csv', header = T)
#View(lg_data$X)
surge_file_details$criteria <- lg_data$criteria[match(surge_file_details$Account.Domain,lg_data$X)]
surge_file_Target_Accounts <- surge_file_details %>% filter( surge_file_details$criteria == "Target")
nrow(surge_file_Target_Accounts)
dim(surge_file_Target_Accounts)
write.csv(surge_file_Target_Accounts,'Final_30_Aug_1.csv')

#Creation of confusion matrix to assess model performance measures
library(caret)
(table(surge_file_details$Hexa_Markup, surge_file_details$criteria))



#Appending DUMP and Newdata for DETAILS
dump_data_details <- read.csv("DETAILS.csv", header = T)
dump_data_details <- dump_data_details[-c(1)]
dump_data_details$Existing.Customer <- as.factor(dump_data_details$Existing.Customer)
str(dump_data_details$Existing.Customer)

new_data <- read.csv("Final_30_Aug.csv", header = T)
new_data <- new_data[-c(1)]
new_data$Existing.Customer <- as.factor(new_data$Existing.Customer)
drop <- c('Account.Rank','criteria')
new_data = new_data[,!(names(new_data) %in% drop)]


#view(past_data)
dim(new_data)
dim(dump_data_details)

names(new_data)
names(dump_data_details)
new_details <- full_join(new_data, dump_data_details)
dim(new_details)
#new[is.na(new_details)] <- 0
write.csv(new_details, "DETAILS_ML.csv")
dim(new_details)
#warnings()





