#set Directory####
setwd("path")

#load library####
library(dplyr)
library(reshape2)
library(MLmetrics)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(corrplot)
library(caTools)
library(xgboost)
library(caret)
library(h2o)
library(Information)


#no scientific notation####
options(scipen=999)

#load data####
data <- read.csv("Data/train.csv")
test <- read.csv("Data/test.csv")

#Initial exploration####

#structure
str(data)
#dates are factors, need to convert

prop.table(table(data$m13)) #99.45:0.55 (Highly skewed)

data$m13 <- as.factor(data$m13)

#class of columns
sapply(data,class)

#distribution
summary_numeric <- data.frame(summary(data[,(sapply(data,class) %in% c("numeric","integer"))]))[,-1]
summary_numeric$metric <- do.call(rbind,strsplit(as.character(summary_numeric$Freq),":"))[,1]
summary_numeric$value <- do.call(rbind,strsplit(as.character(summary_numeric$Freq),":"))[,2]
summary_numeric <- summary_numeric[,-2]

summary_numeric <- dcast(summary_numeric,metric~Var2)

summary(data[,!(sapply(data,class) %in% c("numeric","integer"))])

#Plotting Functions ####
Categorical <- function(data, source_var, target_var){
  p1 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) + geom_bar() +
    scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
    labs(x = source_var, y = target_var) + theme(legend.title = element_blank())
  
  p2 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) + geom_bar(position = "fill") +
    scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
    labs(x = source_var, y = target_var) + theme(legend.title = element_blank())
  x11() 
  grid.arrange(p1, p2)
  
}

Numeric <- function(data, source_var, target_var){
  
  p1 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) +
    geom_histogram(aes(y = ..density..),position = "dodge", col = "black", bins = 30) +
    theme_gdocs() + scale_fill_tableau(name = target_var) + geom_density(alpha = 0.3) +
    labs(x = source_var, y = "density") 
  
  p2 <- ggplot(data, aes(x = data[,c(target_var)], y = data[,c(source_var)], fill = data[,c(target_var)])) +
    geom_boxplot() + theme_gdocs() + scale_fill_tableau(name = target_var) + 
    labs(x = target_var, y = source_var)
  
  x11() 
  grid.arrange(p1, p2)
  
}

#Combining Data with test for new vars ####
names(data)

m13 = data$m13
data$m13 = NULL

#origination_date
data$origination_date <- as.Date(as.character(data$origination_date))
test$origination_date <- as.character(test$origination_date)

test$origination_date <- paste0("20",do.call(rbind,strsplit(as.character(test$origination_date),"/"))[,3],"/",
                                do.call(rbind,strsplit(as.character(test$origination_date),"/"))[,2],"/",
                                do.call(rbind,strsplit(as.character(test$origination_date),"/"))[,1])


test$origination_date  <- as.Date(test$origination_date)

#first_payment_date
data$first_payment_date <- paste0(do.call(rbind,strsplit(as.character(data$first_payment_date),"/"))[,2],"-",
                                  do.call(rbind,strsplit(as.character(data$first_payment_date),"/"))[,1],"-",
                                  "01")

data$first_payment_date <- as.Date(data$first_payment_date)

test$first_payment_date <- as.character(test$first_payment_date)

test$first_payment_date <- as.Date(paste0("20",do.call(rbind,strsplit(as.character(test$first_payment_date),"-"))[,2],"/",
                                          match(do.call(rbind,strsplit(as.character(test$first_payment_date),"-"))[,1],month.abb),"/",
                                          "01"))

data1 <- rbind(data,test)

data$m13 <- m13


#Explore variables ####

#data$loan_id #identifier

#source ##
#Categorical(data,"source","m13")
prop.table(table(data[,c("source","m13")]),1) 

dummy <- as.data.frame(model.matrix(~source, data = data1))
data1 <- cbind(data1[,-which("source" == names(data1))], dummy[,-1])

#financial_institution ##
#Categorical(data,"financial_institution","m13") #club categories
table(data[,c("financial_institution","m13")])

#frequency
fi_freq <- data.frame(prop.table(table(data[,c("financial_institution")]))[order(prop.table(table(data[,c("financial_institution")])))])
names(fi_freq) <- c("Financial_institution","freq")
#response rate
fi_rr <- data.frame(prop.table(table(data[,c("financial_institution","m13")]),1)[order(prop.table(table(data[,c("financial_institution","m13")]),1)[,2]),])[1:19,-2]
names(fi_rr) <- c("Financial_institution","RR")

#freq and rr
fi <- merge(fi_rr,fi_freq,by="Financial_institution")
fi <- fi[order(fi$RR),]
fi$cumfreq <- cumsum(fi$freq)

#New Categories
data1$new_finance_institution <- ifelse((as.character(data1$financial_institution) == as.character(fi$Financial_institution)[1] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[2] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[3]),"fi1",
                                        ifelse((as.character(data1$financial_institution) == as.character(fi$Financial_institution)[4] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[5] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[6]),"fi2",
                                               ifelse((as.character(data1$financial_institution) == as.character(fi$Financial_institution)[7] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[8] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[9]),"fi3",
                                                      ifelse((as.character(data1$financial_institution) == as.character(fi$Financial_institution)[10] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[11] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[12] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[13]),"fi4",
                                                             ifelse((as.character(data1$financial_institution) == as.character(fi$Financial_institution)[14] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[15] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[16] | as.character(data1$financial_institution) == as.character(fi$Financial_institution)[17]),"fi5",
                                                                    "fi6")))))

data1 <- data1[,-which("financial_institution" == names(data1))]

dummy <- as.data.frame(model.matrix(~new_finance_institution, data = data1))
data1 <- cbind(data1[,-which("new_finance_institution" == names(data1))], dummy[,-1])

#interest_rate ##
#Numeric(data,"interest_rate","m13") #important variable (look closely) 

#unpaid_principal_bal (loan amount) ##
#Numeric(data,"unpaid_principal_bal","m13") #lower amount have higher deliquent rate

data1$premium_per_year <- data1$interest_rate*data1$unpaid_principal_bal

#loan_term ##
#Numeric(data,"loan_term","m13") 

data1$total_premium <- data1$premium_per_year*data1$loan_term

unique(data$loan_term)

#new variable ###
data1$first_payment_duration <- as.numeric(data1$first_payment_date - data1$origination_date)

unique(data1$first_payment_duration)

#Numeric(data,"first_payment_duration","m13")

data1$first_payment_duration_mnth <- as.integer(round(data1$first_payment_duration/28,0))
unique(data1$first_payment_duration_mnth)

data1$first_payment_duration <- NULL

data1$first_payment_duration_mnth <- as.factor(data1$first_payment_duration_mnth)

dummy <- as.data.frame(model.matrix(~first_payment_duration_mnth, data = data1))
data1 <- cbind(data1[,-which("first_payment_duration_mnth" == names(data1))], dummy[,-1])

#Numeric(data,"first_payment_duration_mnth","m13")

#loan_to_value ##
#Numeric(data,"loan_to_value","m13") #look at woe

data1$collateral <- data1$unpaid_principal_bal/data1$loan_to_value

#number_of_borrowers ##
#Numeric(data,"number_of_borrowers","m13") #categorize

data1$number_of_borrowers <- as.factor(data1$number_of_borrowers)

dummy <- as.data.frame(model.matrix(~number_of_borrowers, data = data1))
data1 <- cbind(data1[,-which("number_of_borrowers" == names(data1))], dummy[,-1])

names(data1)[which(names(data1) == "dummy[, -1]")] <- names(dummy)[2]

#debt_to_income_ratio
#Numeric(data,"debt_to_income_ratio","m13") #important variable

# (%Collatral/%loan)
data1$debt_income_by_loan_value <- data1$debt_to_income_ratio/data1$loan_to_value

#borrower_credit_score ##
#Numeric(data,"borrower_credit_score","m13") #important variable
unique(data$borrower_credit_score) #analyze 0

data1$credit_score_available <- ifelse(data1$borrower_credit_score == 0,0,1)

# data[data$borrower_credit_score == 0,"borrower_credit_score"] <- 840
# test[test$borrower_credit_score == 0,"borrower_credit_score"] <- 840

#loan_purpose ##
#Categorical(data,"loan_purpose","m13")
prop.table(table(data[,c("loan_purpose","m13")]),1) 

dummy <- as.data.frame(model.matrix(~loan_purpose, data = data1))
data1 <- cbind(data1[,-which("loan_purpose" == names(data1))], dummy[,-1])

#insurance_percent ##
#Numeric(data,"insurance_percent","m13") #look at categories or woe

unique(data1$insurance_percent)

data1$insurance_presence <- ifelse(data1$insurance_percent > 0,1,0)

data1$loan_covered_under_insurance <- data1$unpaid_principal_bal*data1$insurance_percent

#co.borrower_credit_score
#Numeric(data,"co.borrower_credit_score","m13")

data1$max_credit_score <- apply(data1[,c("borrower_credit_score","co.borrower_credit_score")],1,max)
data1$min_credit_score <- apply(data1[,c("borrower_credit_score","co.borrower_credit_score")],1,min)

# data[data$number_of_borrowers == 2 & data$co.borrower_credit_score == 0,"co.borrower_credit_score"]  <- 840
# test[test$number_of_borrowers == 2 & test$co.borrower_credit_score == 0,"co.borrower_credit_score"]  <- 840

#new variable
data1$avg_credit_score <- ifelse(data1$number_of_borrowers2 == 0,
                                data1$borrower_credit_score,
                                (data1$borrower_credit_score + data1$co.borrower_credit_score)/2)

#View(data1[which(data1$avg_credit_score == 0),c("avg_credit_score","credit_score_available")])

#Numeric(data,"avg_credit_score","m13") #important variable

#insurance_type ##
#Categorical(data,"insurance_type","m13")

#m1 to m12
#Numeric(data,"m1","m13")
#Numeric(data,"m2","m13")
#Numeric(data,"m3","m13")
#Numeric(data,"m4","m13")
#Numeric(data,"m5","m13")
#Numeric(data,"m6","m13")
#Numeric(data,"m7","m13")
#Numeric(data,"m8","m13")
#Numeric(data,"m9","m13")
#Numeric(data,"m10","m13")
#Numeric(data,"m11","m13")
#Numeric(data,"m12","m13")

data1$max_deliquent <- apply(data1[,c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12")],1,max)

data1$no_deliquent_last_12mnth <- rowSums(apply(data1[,c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12")],c(1,2),function(x){ifelse(x==0,0,1)}))

data1$no_deliquent_last_6mnth <- rowSums(apply(data1[,c("m7","m8","m9","m10","m11","m12")],c(1,2),function(x){ifelse(x==0,0,1)}))

data1$no_deliquent_last_3mnth <- rowSums(apply(data1[,c("m10","m11","m12")],c(1,2),function(x){ifelse(x==0,0,1)}))

#m1 to m12 (making binary)

data1$m1 <- ifelse(data1$m1 > 0,1,0)
data1$m2 <- ifelse(data1$m2 > 0,1,0)
data1$m3 <- ifelse(data1$m3 > 0,1,0)
data1$m4 <- ifelse(data1$m4 > 0,1,0)
data1$m5 <- ifelse(data1$m5 > 0,1,0)
data1$m6 <- ifelse(data1$m6 > 0,1,0)
data1$m7 <- ifelse(data1$m7 > 0,1,0)
data1$m8 <- ifelse(data1$m8 > 0,1,0)
data1$m9 <- ifelse(data1$m9 > 0,1,0)
data1$m10 <- ifelse(data1$m10 > 0,1,0)
data1$m11 <- ifelse(data1$m11 > 0,1,0)
data1$m12 <- ifelse(data1$m12 > 0,1,0)

#Split data again####

data_train <- data1[1:nrow(data),]
data_test <- data1[(nrow(data)+1):nrow(data1),]

data_train$m13 <- m13

#Explore data_train now ####

str(data_train)

data_train$origination_date <- NULL
data_train$first_payment_date <- NULL
data_train$borrower_credit_score <- NULL
data_train$co.borrower_credit_score <- NULL
data_train$loan_id <- NULL

data_test$origination_date <- NULL
data_test$first_payment_date <- NULL
data_test$borrower_credit_score <- NULL
data_test$co.borrower_credit_score <- NULL
data_test$loan_id <- NULL

str(data_train)

#Numeric(data_train,"premium_per_year","m13")

#Numeric(data_train,"total_premium","m13")

#Numeric(data_train,"collateral","m13")

#Numeric(data_train,"debt_income_by_loan_value","m13")

#Numeric(data_train,"loan_covered_under_insurance","m13")

#Numeric(data_train,"max_credit_score","m13")

#Xgboost model####

#Encoding variables for xgboost

m13 = data_train$m13
label = as.integer(data_train$m13)-1
data_train$m13 = NULL

#Parameters
num_class = length(levels(m13))

params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

#seed
set.seed(123)

#Cross validation
folds = createFolds(m13, k = 5)

cv = lapply(folds, function(x) {
  
  train.data = as.matrix(data_train[-x,])
  train.label = label[-x]
  valid.data = as.matrix(data_train[x,])
  valid.label = label[x]
  
  xgb.train = xgb.DMatrix(data=train.data,label=train.label)
  xgb.valid = xgb.DMatrix(data=valid.data,label=valid.label)
  
  xgb.fit=xgb.train(
    params=params,
    data=xgb.train,
    nrounds=10000,
    nthreads=1,
    early_stopping_rounds=10,
    watchlist=list(val1=xgb.train,val2=xgb.valid),
    verbose=0
  )
  
  xgb.fitted = predict(xgb.fit,train.data,reshape=T)
  xgb.fitted = as.data.frame(xgb.fitted)
  colnames(xgb.fitted) = levels(m13)
  xgb.fitted = cbind.data.frame(xgb.fitted,actual = train.label)
  
  xgb.pred = predict(xgb.fit,valid.data,reshape=T)
  xgb.pred = as.data.frame(xgb.pred)
  colnames(xgb.pred) = levels(m13)
  xgb.pred = cbind.data.frame(xgb.pred,actual = valid.label)
  
  return(list(xgb.fitted,xgb.pred,xgb.fit))

})

#cv train result ####

mn <- min(cv$Fold1[[1]][,2],cv$Fold2[[1]][,2],cv$Fold3[[1]][,2],cv$Fold4[[1]][,2],cv$Fold5[[1]][,2])
mx <- max(cv$Fold1[[1]][,2],cv$Fold2[[1]][,2],cv$Fold3[[1]][,2],cv$Fold4[[1]][,2],cv$Fold5[[1]][,2])

# Selecting cutoff values

#Fold1
cutoff_data <- data.frame(cutoff = 0, TP = 0, TN = 0, FP = 0,FN = 0)
cutoffs <- seq(mn,mx,length=200)

for(cutoff in cutoffs){
  predicted <- as.numeric(cv$Fold1[[1]][,2] > cutoff)
  TP = sum(predicted==1 & cv$Fold1[[1]][,3]==1)
  TN = sum(predicted==0 & cv$Fold1[[1]][,3]==0)
  FP = sum(predicted==1 & cv$Fold1[[1]][,3]==0)
  FN = sum(predicted==0 & cv$Fold1[[1]][,3]==1)
  cutoff_data <- rbind(cutoff_data, c(cutoff, TP, TN, FP, FN))
}
cutoff_data <- cutoff_data[-1,]

# calculate metrics
cutoff_data <- cutoff_data %>% mutate(P = TP+FN, N = TN+FP)
cutoff_data <- cutoff_data %>% mutate(Accuracy = (TP+TN)/(P+N),
                                      Precision = TP/(TP+FP),
                                      Recall = TP/(TP+FN))
cutoff_data <- cutoff_data %>% mutate(F1_score = 2*(Precision*Recall)/(Precision+Recall))

cutoff_cv <- cutoff_data[,c(1,11)]
names(cutoff_cv) <- c("cutoff","fold1")

#Fold 2
cutoff_data <- data.frame(cutoff = 0, TP = 0, TN = 0, FP = 0,FN = 0)
cutoffs <- seq(mn,mx,length=200)

for(cutoff in cutoffs){
  predicted <- as.numeric(cv$Fold2[[1]][,2] > cutoff)
  TP = sum(predicted==1 & cv$Fold2[[1]][,3]==1)
  TN = sum(predicted==0 & cv$Fold2[[1]][,3]==0)
  FP = sum(predicted==1 & cv$Fold2[[1]][,3]==0)
  FN = sum(predicted==0 & cv$Fold2[[1]][,3]==1)
  cutoff_data <- rbind(cutoff_data, c(cutoff, TP, TN, FP, FN))
}
cutoff_data <- cutoff_data[-1,]

# calculate metrics
cutoff_data <- cutoff_data %>% mutate(P = TP+FN, N = TN+FP)
cutoff_data <- cutoff_data %>% mutate(Accuracy = (TP+TN)/(P+N),
                                      Precision = TP/(TP+FP),
                                      Recall = TP/(TP+FN))
cutoff_data <- cutoff_data %>% mutate(F1_score = 2*(Precision*Recall)/(Precision+Recall))

cutoff_cv <- cbind(cutoff_cv,fold2 = cutoff_data[,11])

#Fold 3
cutoff_data <- data.frame(cutoff = 0, TP = 0, TN = 0, FP = 0,FN = 0)
cutoffs <- seq(mn,mx,length=200)

for(cutoff in cutoffs){
  predicted <- as.numeric(cv$Fold3[[1]][,2] > cutoff)
  TP = sum(predicted==1 & cv$Fold3[[1]][,3]==1)
  TN = sum(predicted==0 & cv$Fold3[[1]][,3]==0)
  FP = sum(predicted==1 & cv$Fold3[[1]][,3]==0)
  FN = sum(predicted==0 & cv$Fold3[[1]][,3]==1)
  cutoff_data <- rbind(cutoff_data, c(cutoff, TP, TN, FP, FN))
}
cutoff_data <- cutoff_data[-1,]

# calculate metrics
cutoff_data <- cutoff_data %>% mutate(P = TP+FN, N = TN+FP)
cutoff_data <- cutoff_data %>% mutate(Accuracy = (TP+TN)/(P+N),
                                      Precision = TP/(TP+FP),
                                      Recall = TP/(TP+FN))
cutoff_data <- cutoff_data %>% mutate(F1_score = 2*(Precision*Recall)/(Precision+Recall))

cutoff_cv <- cbind(cutoff_cv,fold3 = cutoff_data[,11])

#Fold 4
cutoff_data <- data.frame(cutoff = 0, TP = 0, TN = 0, FP = 0,FN = 0)
cutoffs <- seq(mn,mx,length=200)

for(cutoff in cutoffs){
  predicted <- as.numeric(cv$Fold4[[1]][,2] > cutoff)
  TP = sum(predicted==1 & cv$Fold4[[1]][,3]==1)
  TN = sum(predicted==0 & cv$Fold4[[1]][,3]==0)
  FP = sum(predicted==1 & cv$Fold4[[1]][,3]==0)
  FN = sum(predicted==0 & cv$Fold4[[1]][,3]==1)
  cutoff_data <- rbind(cutoff_data, c(cutoff, TP, TN, FP, FN))
}
cutoff_data <- cutoff_data[-1,]

# calculate metrics
cutoff_data <- cutoff_data %>% mutate(P = TP+FN, N = TN+FP)
cutoff_data <- cutoff_data %>% mutate(Accuracy = (TP+TN)/(P+N),
                                      Precision = TP/(TP+FP),
                                      Recall = TP/(TP+FN))
cutoff_data <- cutoff_data %>% mutate(F1_score = 2*(Precision*Recall)/(Precision+Recall))

cutoff_cv <- cbind(cutoff_cv,fold4 = cutoff_data[,11])

#Fold 5
cutoff_data <- data.frame(cutoff = 0, TP = 0, TN = 0, FP = 0,FN = 0)
cutoffs <- seq(mn,mx,length=200)

for(cutoff in cutoffs){
  predicted <- as.numeric(cv$Fold5[[1]][,2] > cutoff)
  TP = sum(predicted==1 & cv$Fold5[[1]][,3]==1)
  TN = sum(predicted==0 & cv$Fold5[[1]][,3]==0)
  FP = sum(predicted==1 & cv$Fold5[[1]][,3]==0)
  FN = sum(predicted==0 & cv$Fold5[[1]][,3]==1)
  cutoff_data <- rbind(cutoff_data, c(cutoff, TP, TN, FP, FN))
}
cutoff_data <- cutoff_data[-1,]

# calculate metrics
cutoff_data <- cutoff_data %>% mutate(P = TP+FN, N = TN+FP)
cutoff_data <- cutoff_data %>% mutate(Accuracy = (TP+TN)/(P+N),
                                      Precision = TP/(TP+FP),
                                      Recall = TP/(TP+FN))
cutoff_data <- cutoff_data %>% mutate(F1_score = 2*(Precision*Recall)/(Precision+Recall))

cutoff_cv <- cbind(cutoff_cv,fold5 = cutoff_data[,11])

cutoff_cv$avg_f1 <- rowMeans(cutoff_cv[,-1])

cutoff_max_F1_train1 <- cutoff_cv$cutoff[which.max(cutoff_cv$fold1)]
cutoff_max_F1_train2 <- cutoff_cv$cutoff[which.max(cutoff_cv$fold2)]
cutoff_max_F1_train3 <- cutoff_cv$cutoff[which.max(cutoff_cv$fold3)]
cutoff_max_F1_train4 <- cutoff_cv$cutoff[which.max(cutoff_cv$fold4)]
cutoff_max_F1_train5 <- cutoff_cv$cutoff[which.max(cutoff_cv$fold5)]


cutoff_max_F1 <- cutoff_cv$cutoff[which.max(cutoff_cv$avg_f1)]
#0.35

#cv valid result ####

mn <- min(cv$Fold1[[1]][,2],cv$Fold2[[1]][,2],cv$Fold3[[1]][,2],cv$Fold4[[1]][,2],cv$Fold5[[1]][,2])
mx <- max(cv$Fold1[[1]][,2],cv$Fold2[[1]][,2],cv$Fold3[[1]][,2],cv$Fold4[[1]][,2],cv$Fold5[[1]][,2])

# Selecting cutoff values

#Fold1
cutoff_data <- data.frame(cutoff = 0, TP = 0, TN = 0, FP = 0,FN = 0)
cutoffs <- seq(mn,mx,length=200)

for(cutoff in cutoffs){
  predicted <- as.numeric(cv$Fold1[[2]][,2] > cutoff)
  TP = sum(predicted==1 & cv$Fold1[[2]][,3]==1)
  TN = sum(predicted==0 & cv$Fold1[[2]][,3]==0)
  FP = sum(predicted==1 & cv$Fold1[[2]][,3]==0)
  FN = sum(predicted==0 & cv$Fold1[[2]][,3]==1)
  cutoff_data <- rbind(cutoff_data, c(cutoff, TP, TN, FP, FN))
}
cutoff_data <- cutoff_data[-1,]

# calculate metrics
cutoff_data <- cutoff_data %>% mutate(P = TP+FN, N = TN+FP)
cutoff_data <- cutoff_data %>% mutate(Accuracy = (TP+TN)/(P+N),
                                      Precision = TP/(TP+FP),
                                      Recall = TP/(TP+FN))
cutoff_data <- cutoff_data %>% mutate(F1_score = 2*(Precision*Recall)/(Precision+Recall))

cutoff_cv_valid <- cutoff_data[,c(1,11)]
names(cutoff_cv_valid) <- c("cutoff","fold1")

#Fold2
cutoff_data <- data.frame(cutoff = 0, TP = 0, TN = 0, FP = 0,FN = 0)
cutoffs <- seq(mn,mx,length=200)

for(cutoff in cutoffs){
  predicted <- as.numeric(cv$Fold2[[2]][,2] > cutoff)
  TP = sum(predicted==1 & cv$Fold2[[2]][,3]==1)
  TN = sum(predicted==0 & cv$Fold2[[2]][,3]==0)
  FP = sum(predicted==1 & cv$Fold2[[2]][,3]==0)
  FN = sum(predicted==0 & cv$Fold2[[2]][,3]==1)
  cutoff_data <- rbind(cutoff_data, c(cutoff, TP, TN, FP, FN))
}
cutoff_data <- cutoff_data[-1,]

# calculate metrics
cutoff_data <- cutoff_data %>% mutate(P = TP+FN, N = TN+FP)
cutoff_data <- cutoff_data %>% mutate(Accuracy = (TP+TN)/(P+N),
                                      Precision = TP/(TP+FP),
                                      Recall = TP/(TP+FN))
cutoff_data <- cutoff_data %>% mutate(F1_score = 2*(Precision*Recall)/(Precision+Recall))

cutoff_cv_valid <- cbind(cutoff_cv_valid,fold2 = cutoff_data[,11])

#Fold3
cutoff_data <- data.frame(cutoff = 0, TP = 0, TN = 0, FP = 0,FN = 0)
cutoffs <- seq(mn,mx,length=200)

for(cutoff in cutoffs){
  predicted <- as.numeric(cv$Fold3[[2]][,2] > cutoff)
  TP = sum(predicted==1 & cv$Fold3[[2]][,3]==1)
  TN = sum(predicted==0 & cv$Fold3[[2]][,3]==0)
  FP = sum(predicted==1 & cv$Fold3[[2]][,3]==0)
  FN = sum(predicted==0 & cv$Fold3[[2]][,3]==1)
  cutoff_data <- rbind(cutoff_data, c(cutoff, TP, TN, FP, FN))
}
cutoff_data <- cutoff_data[-1,]

# calculate metrics
cutoff_data <- cutoff_data %>% mutate(P = TP+FN, N = TN+FP)
cutoff_data <- cutoff_data %>% mutate(Accuracy = (TP+TN)/(P+N),
                                      Precision = TP/(TP+FP),
                                      Recall = TP/(TP+FN))
cutoff_data <- cutoff_data %>% mutate(F1_score = 2*(Precision*Recall)/(Precision+Recall))

cutoff_cv_valid <- cbind(cutoff_cv_valid,fold3 = cutoff_data[,11])

#Fold4
cutoff_data <- data.frame(cutoff = 0, TP = 0, TN = 0, FP = 0,FN = 0)
cutoffs <- seq(mn,mx,length=200)

for(cutoff in cutoffs){
  predicted <- as.numeric(cv$Fold4[[2]][,2] > cutoff)
  TP = sum(predicted==1 & cv$Fold4[[2]][,3]==1)
  TN = sum(predicted==0 & cv$Fold4[[2]][,3]==0)
  FP = sum(predicted==1 & cv$Fold4[[2]][,3]==0)
  FN = sum(predicted==0 & cv$Fold4[[2]][,3]==1)
  cutoff_data <- rbind(cutoff_data, c(cutoff, TP, TN, FP, FN))
}
cutoff_data <- cutoff_data[-1,]

# calculate metrics
cutoff_data <- cutoff_data %>% mutate(P = TP+FN, N = TN+FP)
cutoff_data <- cutoff_data %>% mutate(Accuracy = (TP+TN)/(P+N),
                                      Precision = TP/(TP+FP),
                                      Recall = TP/(TP+FN))
cutoff_data <- cutoff_data %>% mutate(F1_score = 2*(Precision*Recall)/(Precision+Recall))

cutoff_cv_valid <- cbind(cutoff_cv_valid,fold4 = cutoff_data[,11])

#Fold5
cutoff_data <- data.frame(cutoff = 0, TP = 0, TN = 0, FP = 0,FN = 0)
cutoffs <- seq(mn,mx,length=200)

for(cutoff in cutoffs){
  predicted <- as.numeric(cv$Fold5[[2]][,2] > cutoff)
  TP = sum(predicted==1 & cv$Fold5[[2]][,3]==1)
  TN = sum(predicted==0 & cv$Fold5[[2]][,3]==0)
  FP = sum(predicted==1 & cv$Fold5[[2]][,3]==0)
  FN = sum(predicted==0 & cv$Fold5[[2]][,3]==1)
  cutoff_data <- rbind(cutoff_data, c(cutoff, TP, TN, FP, FN))
}
cutoff_data <- cutoff_data[-1,]

# calculate metrics
cutoff_data <- cutoff_data %>% mutate(P = TP+FN, N = TN+FP)
cutoff_data <- cutoff_data %>% mutate(Accuracy = (TP+TN)/(P+N),
                                      Precision = TP/(TP+FP),
                                      Recall = TP/(TP+FN))
cutoff_data <- cutoff_data %>% mutate(F1_score = 2*(Precision*Recall)/(Precision+Recall))

cutoff_cv_valid <- cbind(cutoff_cv_valid,fold5 = cutoff_data[,11])

cutoff_cv_valid$avg_f1 <- rowMeans(cutoff_cv_valid[,-1])

cutoff_max_F1_valid1 <- cutoff_cv_valid$cutoff[which.max(cutoff_cv_valid$fold1)]
cutoff_max_F1_valid2 <- cutoff_cv_valid$cutoff[which.max(cutoff_cv_valid$fold2)]
cutoff_max_F1_valid3 <- cutoff_cv_valid$cutoff[which.max(cutoff_cv_valid$fold3)]
cutoff_max_F1_valid4 <- cutoff_cv_valid$cutoff[which.max(cutoff_cv_valid$fold4)]
cutoff_max_F1_valid5 <- cutoff_cv_valid$cutoff[which.max(cutoff_cv_valid$fold5)]

cutoff_max_F1_valid <- cutoff_cv_valid$cutoff[which.max(cutoff_cv_valid$avg_f1)]
#0.23

# Model Output ####
test.data <- as.matrix(data_test)

xgb.pred1 = predict(cv$Fold1[[3]],test.data,reshape=T)
xgb.pred1 = as.data.frame(xgb.pred1)
colnames(xgb.pred1) = levels(m13)


xgb.pred2 = predict(cv$Fold2[[3]],test.data,reshape=T)
xgb.pred2 = as.data.frame(xgb.pred2)
colnames(xgb.pred2) = levels(m13)


xgb.pred3 = predict(cv$Fold3[[3]],test.data,reshape=T)
xgb.pred3 = as.data.frame(xgb.pred3)
colnames(xgb.pred3) = levels(m13)


xgb.pred4 = predict(cv$Fold4[[3]],test.data,reshape=T)
xgb.pred4 = as.data.frame(xgb.pred4)
colnames(xgb.pred4) = levels(m13)


xgb.pred5 = predict(cv$Fold5[[3]],test.data,reshape=T)
xgb.pred5 = as.data.frame(xgb.pred5)
colnames(xgb.pred5) = levels(m13)


#0.23 cutoff

y_pred_test1 <- ifelse(xgb.pred1[,2] > cutoff_max_F1_valid1,1,0)
y_pred_test2 <- ifelse(xgb.pred2[,2] > cutoff_max_F1_valid2,1,0)
y_pred_test3 <- ifelse(xgb.pred3[,2] > cutoff_max_F1_valid3,1,0)
y_pred_test4 <- ifelse(xgb.pred4[,2] > cutoff_max_F1_valid4,1,0)
y_pred_test5 <- ifelse(xgb.pred5[,2] > cutoff_max_F1_valid5,1,0)

y_pred <- cbind.data.frame(y_pred_test1,y_pred_test2,y_pred_test3,y_pred_test4,y_pred_test5)
y_pred$sum <- rowSums(y_pred)

table(y_pred$sum)
y_pred$final_pred <- ifelse(y_pred$sum > 2,1,0)

output49 <- data.frame(cbind(loan_id = test$loan_id,m13 = y_pred$final_pred))
write.csv(output49,"Outputs/Submission49.csv",row.names=F)

y_pred$final_pred <- ifelse(y_pred$sum > 1,1,0)

output50 <- data.frame(cbind(loan_id = test$loan_id,m13 = y_pred$final_pred))
write.csv(output50,"Outputs/Submission50.csv",row.names=F)

y_pred$final_pred <- ifelse(y_pred$sum > 0,1,0)

output51 <- data.frame(cbind(loan_id = test$loan_id,m13 = y_pred$final_pred))
write.csv(output51,"Outputs/Submission51.csv",row.names=F)

#Explore Model test output####

table(y_pred_test1)
table(y_pred_test2)
table(y_pred_test3)
table(y_pred_test4)
table(y_pred_test5)

table(y_pred$final_pred)

# 0.30 cutoff ####

y_pred_test1 <- ifelse(xgb.pred1[,2] > 0.30,1,0)
y_pred_test2 <- ifelse(xgb.pred2[,2] > 0.30,1,0)
y_pred_test3 <- ifelse(xgb.pred3[,2] > 0.30,1,0)
y_pred_test4 <- ifelse(xgb.pred4[,2] > 0.30,1,0)
y_pred_test5 <- ifelse(xgb.pred5[,2] > 0.30,1,0)

y_pred <- cbind.data.frame(y_pred_test1,y_pred_test2,y_pred_test3,y_pred_test4,y_pred_test5)
y_pred$sum <- rowSums(y_pred)

table(y_pred$sum)

y_pred$final_pred <- ifelse(y_pred$sum > 0,1,0)

output66 <- data.frame(cbind(loan_id = test$loan_id,m13 = y_pred$final_pred))
write.csv(output66,"Outputs/Submission115.csv",row.names=F)
