library(caret)
library(dplyr)
library(rio)
library(taRifx)
library("qdapRegex")

data_list <- read.csv("DETAILS.csv", header = T)
dim(data_list)

surge_file_details <- data_list[c('Account.Domain',
                                     'overall_score','Account..Demandbase.Qualification.Score',
                                     'session_duration', 
                                   'Percent.Increase.Traffic.MoM','Sessions',
                                     
                                     'Page_views','Bounce_Rate','Source')]
dim(surge_file_details)
str(surge_file_details)



#Replacing NA to zero
surge_file <- surge_file_details
surge_file[is.na(surge_file)] = 0


#Creating Mean for all above important variables 
Length_Bounce_Rate <-length(which(surge_file$Bounce_Rate != 0))
Mean_Bounce_Rate <- sum(surge_file$Bounce_Rate, na.rm = TRUE) 
Mean_Bounce_Rate <- Mean_Bounce_Rate/Length_Bounce_Rate
Mean_Bounce_Rate


length_session_duration <- length(which(surge_file$session_duration != 0))
Mean_session_duration <- sum(surge_file$session_duration, na.rm = TRUE) 
Mean_session_duration <- Mean_session_duration/length_session_duration
Mean_session_duration

Length_overall_score <- length(which(surge_file$overall_score != 0))
Mean_overall_score <- sum(surge_file$overall_score, na.rm = TRUE) 
Mean_overall_score <- Mean_overall_score/Length_overall_score
Mean_overall_score

#Creating a derived Variable Hexa_Markup

surge_file$Hexa_Markup <- ifelse((surge_file$Bounce_Rate < Mean_Bounce_Rate) &
                                  (surge_file$session_duration > Mean_session_duration) &
                                   (surge_file$overall_score > Mean_overall_score) , 1, 0)
surge_file$Hexa_Markup <- as.factor(surge_file$Hexa_Markup)
#View(surge_file$Hexa_Markup)
write.csv(surge_file, "Test.csv")  
names(surge_file)





###########################################################################
surge_file_info <- surge_file
set.seed(1522)
trainindex <- createDataPartition(surge_file_info$Hexa_Markup, p=0.9, list = F)
train_df <- surge_file_info[trainindex,]
test_df <- surge_file_info[-trainindex,]
nrow(test_df)
nrow(train_df)
str(train_df$Hexa_Markup)
lg_train_df <- as.data.frame(train_df)
#View(lg_train_df)
#View(offpage_surge_account_info)
lg_test_df <- as.data.frame(test_df)


#View(lg_test_df)
#write.csv(lg_train_df,'C:/Users/Admin/Desktop/Desktop/Till/VJ_PGDM/3rd_Sem/BA/Logistic Reg 10 Sep/sSurge_train.csv')
lg_train_df$Account..Demandbase.Qualification.Score

model = glm(Hexa_Markup ~ ., family = binomial(link = 'logit'),
data = lg_train_df[, !(colnames(lg_train_df) %in% c("Account.Domain","Source","Account..Demandbase.Qualification.Score"))], maxit = 100)
summary(model)
fitted_prob <- predict(model, lg_test_df[, !(colnames(lg_train_df) %in% c("Account.Domain","Source","Account..Demandbase.Qualification.Score"))], type = 'response')

#fitte_results <- ifelse(fitted_prob > threshold, 1, 0)
#Verification
#misclasserror <- mean(fitte_results != lg_test_df$Hexa_Markup)
#print(1 - misclasserror)

## Prediction on Development Dataset WITH aLL VARIABLES 
library(ROCR)
lg_test_df$prediction=predict(model, lg_test_df[, !(colnames(lg_train_df) %in% c("Account.Domain","Source","Account..Demandbase.Qualification.Score"))], type = 'response')
ROCRpred = prediction(lg_test_df$prediction, lg_test_df$Hexa_Markup)
threshold = as.numeric(performance(ROCRpred, "auc")@y.values)
threshold

a <- table(lg_test_df$Hexa_Markup, fitted_prob>threshold)
a
accuracy <- (a[[1,1]] + a[[2,2]]) / (a[[1,1]] + a[[1,2]] + a[[2,1]] + a[[2,2]])
accuracy


#View(lg_test_df)
#lg_test_df[criteria<0.5, "criteria"] <- "Not Eligible"
#lg_test_df[criteria>=0.5, "criteria"] <- "Eligible"
lg_test_df[fitted_prob<threshold, "fitted_prob"] <- "Not"
lg_test_df[fitted_prob>=threshold, "fitted_prob"] <- "Target"
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





