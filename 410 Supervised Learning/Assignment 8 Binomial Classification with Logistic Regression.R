##Assignment 8: Binary Classification with Logistic Regression

options(scipen = 999)
setwd("C:/Users/KJohn/OneDrive/Desktop/Northwestern/MSDS 410 Data Modeling for Supervised Learning/Assignment 8 Logistic Regression")
my.data <- read.csv("UniversalBank.csv", header = TRUE)
str(my.data)
head(my.data)

###################################
##(1) Split the Sample Population##
###################################

set.seed(12345)
my.data$u <- runif(n=dim(my.data)[1],min=0,max=1)

train.df <- subset(my.data, u<0.70)
test.df <- subset(my.data, u>=0.70)

# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(my.data)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)

Set <- c('Train', 'Test', 'Total')
dim.train <- dim(train.df)[1]
dim.test <- dim(test.df)[1]
dim.total <- dim.train + dim.test
N <- c(dim.train, dim.test, dim.total)
pct.train <- dim.train/dim.total
pct.test <- dim.test/dim.total
pct.total <- dim.total/5000
Pcts <- c(pct.train, pct.test, pct.total)
Table.df <- data.frame(Set, N, Pcts)
Table.df

#Drop Zip Code from the dataset
drop.list <- c('ZIP.Code', 'u')
train.clean <-train.df[,!(names(my.data) %in% drop.list)]
train.clean.copy <- train.clean
str(train.clean)

#################################
##(2) Exploratory Data Analysis##
#################################

# Response rates for discrete variables
response.Education <- aggregate(train.clean$PersonalLoan,
                                by=list(Education=train.clean$Education),
                                FUN=mean)
response.Securities <- aggregate(train.clean$PersonalLoan,
                                by=list(Securities=train.clean$SecuritiesAccount),
                                FUN=mean)
response.CDAccount <- aggregate(train.clean$PersonalLoan,
                                by=list(CDAccount=train.clean$CDAccount),
                                FUN=mean)
response.Online <- aggregate(train.clean$PersonalLoan,
                                by=list(Online=train.clean$Online),
                                FUN=mean)
response.CreditCard <- aggregate(train.clean$PersonalLoan,
                                by=list(CreditCard=train.clean$CreditCard),
                                FUN=mean)
barplot.Education <- barplot(height=response.Education$x,names.arg=response.Education$Education,
                      xlab='Education Level',ylab='Response Rate')
barplot.Securities <- barplot(height=response.Securities$x,names.arg=response.Securities$Securities,
                             xlab='Securities Account',ylab='Response Rate')
barplot.CDAccount <- barplot(height=response.CDAccount$x,names.arg=response.CDAccount$CDAccount,
                             xlab='CD Account',ylab='Response Rate')
barplot.Online <- barplot(height=response.Online$x,names.arg=response.Online$Online,
                             xlab='Online',ylab='Response Rate')
barplot.CreditCard <- barplot(height=response.CreditCard$x,names.arg=response.CreditCard$ECreditCard,
                             xlab='Credit Card',ylab='Response Rate')

#Discretize continuous variables
train.clean$Age_Bins <- cut(train.clean$Age,breaks=11) 
table(train.clean$Age_Bins)
response.Age_Bins <- aggregate(train.clean$PersonalLoan,
                               by=list(Age_Bins=train.clean$Age_Bins),
                               FUN=mean)
barplot(height=response.Age_Bins$x,names.arg=response.Age_Bins$Age_Bins,
        xlab='Age_Bin',ylab='Response Rate',las=2,cex.names=0.75)

train.clean$Experience_Bins <- cut(train.clean$Experience,breaks=8) 
table(train.clean$Experience_Bins)
response.Experience_Bins <- aggregate(train.clean$PersonalLoan,
                                      by=list(Experience_Bins=train.clean$Experience_Bins),
                                      FUN=mean)
barplot(height=response.Experience_Bins$x,names.arg=response.Experience_Bins$Experience_Bins,
        xlab='Experience_Bin',ylab='Response Rate',las=2,cex.names=0.75)

train.clean$Income_Bins <- cut(train.clean$Income,breaks=6) 
table(train.clean$Income_Bins)
response.Income_Bins <- aggregate(train.clean$PersonalLoan,
                                  by=list(Income_Bins=train.clean$Income_Bins),
                                  FUN=mean)
barplot(height=response.Income_Bins$x,names.arg=response.Income_Bins$Income_Bins,
        xlab='Income_Bin',ylab='Response Rate',las=2,cex.names=0.75)

train.clean$Family_Bins <- cut(train.clean$Family,breaks=3) 
table(train.clean$Family_Bins)
response.Family_Bins <- aggregate(train.clean$PersonalLoan,
                                  by=list(Family_Bins=train.clean$Family_Bins),
                                  FUN=mean)
barplot(height=response.Family_Bins$x,names.arg=response.Family_Bins$Family_Bins,
        xlab='Family_Bin',ylab='Response Rate',las=2,cex.names=0.75)

train.clean$CCAvg_Bins <- cut(train.clean$CCAvg,breaks=20) 
table(train.clean$CCAvg_Bins)
response.CCAvg_Bins <- aggregate(train.clean$PersonalLoan,
                                 by=list(CCAvg_Bins=train.clean$CCAvg_Bins),
                                 FUN=mean)
barplot(height=response.CCAvg_Bins$x,names.arg=response.CCAvg_Bins$CCAvg_Bins,
        xlab='CCAvg_Bin',ylab='Response Rate',las=2,cex.names=0.75)

train.clean$Mortgage_Bins <- cut(train.clean$Mortgage,breaks=6) 
table(train.clean$Mortgage_Bins)
response.Mortgage_Bins <- aggregate(train.clean$PersonalLoan,
                                    by=list(Mortgage_Bins=train.clean$Mortgage_Bins),
                                    FUN=mean)
barplot(height=response.Mortgage_Bins$x,names.arg=response.Mortgage_Bins$Mortgage_Bins,
        xlab='Mortgage_Bin',ylab='Response Rate',las=2,cex.names=0.75)

############################
##(3) Model 1: Naive Model##
############################

model.1 <- glm(PersonalLoan ~ Income+CCAvg+CDAccount+factor(Education)+Family
               +SecuritiesAccount, data=train.clean, family=c('binomial'))

summary(model.1)

library(pROC)
roc.1 <- roc(response=train.clean$PersonalLoan, predictor=model.1$fitted.values)
print(roc.1)
plot(roc.1)

# Compute AUC
auc.1 <- auc(roc.1);

#> auc.1
#Area under the curve is 0.9584

#Find threshold value of ROC Curve
roc.specs.1 <- coords(roc=roc.1,x=c('best'),
                    input=c('threshold','specificity','sensitivity'),
                    ret=c('threshold','specificity','sensitivity'),
                    as.list=TRUE)

#Assign classes
train.df$Model.Scores.1 <- model.1$fitted.values;
train.df$classes <- ifelse(train.df$Model.Scores.1>roc.specs.1$threshold,1,0);

# Rough confusion matrix using counts;
table(train.df$PersonalLoan, train.df$classes)
#Proper confusion matrix using rates
t <- table(train.df$PersonalLoan, train.df$classes);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r


##########################
##(4a) Model 2: Step AIC##
##########################

library(MASS)
model.2.AIC <- glm(PersonalLoan ~ ., data = train.clean)
model.2 <- stepAIC(model.2.AIC, direction = "both")
summary(model.2)

roc.2 <- roc(response=train.clean$PersonalLoan, predictor=model.2$fitted.values)
print(roc.2)
plot(roc.2)

# Compute AUC
auc.2 <- auc(roc.2)

#> auc.2
#Area under the curve is 0.9678

#Find threshold value of ROC Curve
roc.specs.2 <- coords(roc=roc.2,x=c('best'),
                     input=c('threshold','specificity','sensitivity'),
                     ret=c('threshold','specificity','sensitivity'),
                     as.list=TRUE)

#Assign classes
train.df$ModelScores.2 <- model.2$fitted.values;
train.df$classes2 <- ifelse(train.df$ModelScores.2>roc.specs.2$threshold,1,0);

# Rough confusion matrix using counts;
table(train.df$PersonalLoan, train.df$classes2)
#Proper confusion matrix using rates
t <- table(train.df$PersonalLoan, train.df$classes2);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

#####################################
##(4b) Model 3: Step AIC & Get Bins##
#####################################

library(logiBin)
str(train.clean.copy)
xvars <- c('Age', 'Experience', 'Income', 'Family', 'CCAvg', 'Education', 'Mortgage')
new.bins <- getBins(train.clean.copy, 'PersonalLoan', xvars, minProp = 0.03, minCr = 0.9, nCores = 1)
new.bins
train.new <- train.clean.copy

#Income Bins
breaks <- c(1,2,4)
tags <- c(("1-2"), ("3-4"))
train.new$Family.Bins <- cut(train.clean.copy$Family,
                             breaks = breaks,
                             include.lowest = TRUE,
                             right = FALSE,
                             labels = tags)
response.Family.New.Bins <- aggregate(train.new$PersonalLoan,
                                    by=list(Family.New.Bins=train.new$Family.Bins),
                                    FUN=mean)
barplot(height=response.Family.New.Bins$x,names.arg=response.Family.New.Bins$Family.New.Bins,
        xlab='Family Bins',ylab='Response Rate',las=2,cex.names=0.75)

#Income Bins
breaks <- c(0,63,82,92,114,163,224)
tags <- c(("8-63"), ("64-82"), ("83-92"), ("93-114"), ("115-163"), ("164-224"))
train.new$Income.Bins <- cut(train.clean.copy$Income,
                                         breaks = breaks,
                                         include.lowest = TRUE,
                                         right = FALSE,
                                         labels = tags)
response.Income.New.Bins <- aggregate(train.new$PersonalLoan,
                                      by=list(Income.New.Bins=train.new$Income.Bins),
                                      FUN=mean)
barplot(height=response.Income.New.Bins$x,names.arg=response.Income.New.Bins$Income.New.Bins,
        xlab='Family Bins',ylab='Response Rate',las=2,cex.names=0.75)

#CCAvg Bins
breaks <- c(0,2.5,2.9,10)
tags <- c(("0-2.5"), ("2.6-2.9"), ("3-10"))
train.new$CCAvg.Bins <- cut(train.clean.copy$CCAvg,
                             breaks = breaks,
                             include.lowest = TRUE,
                             right = FALSE,
                             labels = tags)
response.CCAvg.New.Bins <- aggregate(train.new$PersonalLoan,
                                      by=list(CCAvg.New.Bins=train.new$CCAvg.Bins),
                                      FUN=mean)
barplot(height=response.CCAvg.New.Bins$x,names.arg=response.CCAvg.New.Bins$CCAvg.New.Bins,
        xlab='Family Bins',ylab='Response Rate',las=2,cex.names=0.75)

#Education Bins
breaks <- c(1,2,3)
tags <- c(("1"), ("2-3"))
train.new$Education.Bins <- cut(train.clean.copy$Education,
                            breaks = breaks,
                            include.lowest = TRUE,
                            right = FALSE,
                            labels = tags)
response.Education.New.Bins <- aggregate(train.new$PersonalLoan,
                                     by=list(Education.New.Bins=train.new$Education.Bins),
                                     FUN=mean)
barplot(height=response.Education.New.Bins$x,names.arg=response.Education.New.Bins$Education.New.Bins,
        xlab='Family Bins',ylab='Response Rate',las=2,cex.names=0.75)

#Mortgage Bins
breaks <- c(0,290,617)
tags <- c(("0-290"), ("291-617"))
train.new$Mortgage.Bins <- cut(train.clean.copy$Mortgage,
                                breaks = breaks,
                                include.lowest = TRUE,
                                right = FALSE,
                                labels = tags)
response.Mortgage.New.Bins <- aggregate(train.new$PersonalLoan,
                                         by=list(Mortgage.New.Bins=train.new$Mortgage.Bins),
                                         FUN=mean)
barplot(height=response.Mortgage.New.Bins$x,names.arg=response.Mortgage.New.Bins$Mortgage.New.Bins,
        xlab='Family Bins',ylab='Response Rate',las=2,cex.names=0.75)

drop.list <- c('Income', 'Family', 'CCAvg', 'Education', 'Mortgage')
train.new <-train.new[,!(names(train.new) %in% drop.list)]
str(train.new)

model.3 <- glm(PersonalLoan ~ ., data = train.new)
model.3 <- stepAIC(model.3, direction = "both")
summary(model.3)

roc.3 <- roc(response=train.new$PersonalLoan, predictor=model.3$fitted.values)
print(roc.3)
plot(roc.3)

# Compute AUC
auc.3 <- auc(roc.3)

#> auc.3
#Area under the curve is 0.9678

#Find threshold value of ROC Curve
roc.specs.3 <- coords(roc=roc.3,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE)

#Assign classes
train.df$ModelScores.3 <- model.3$fitted.values;
train.df$classes3 <- ifelse(train.df$ModelScores.3>roc.specs.3$threshold,1,0);

# Rough confusion matrix using counts;
table(train.df$PersonalLoan, train.df$classes3)
#Proper confusion matrix using rates
t <- table(train.df$PersonalLoan, train.df$classes3);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

######################
##5 Model Comparison##
######################

Model <- c('Model 1', 'Model 2', 'Model 3')
AUC <- c(.9584, .9678, .9661)
True.Positive <- c(.8517, .9215, .8895)
True.Negative <- c(.9399, .9104, .9295)
insample.df <- data.frame(Model, AUC, True.Positive, True.Negative)
insample.df

drop.list <- c('ZIP.Code', 'u')
test.clean <-test.df[,!(names(my.data) %in% drop.list)]
test.clean.copy <- test.clean
test.new <- test.clean.copy

##############################
#Model 1 Predictive Accuracy##
##############################
model.1.test <- predict(model.1, newdata = test.clean, type = 'response')
summary(model.1.test)

roc.1.test <- roc(response=test.clean$PersonalLoan, predictor=model.1.test)
print(roc.1.test)
plot(roc.1.test)

# Compute AUC
auc.1.test <- auc(roc.1.test);

#> auc.1.test
#Area under the curve is 0.9551

#Find threshold value of ROC Curve
roc.specs.1.test <- coords(roc=roc.1.test,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE)

#Assign classes
test.df$Model.Scores.1 <- model.1.test
test.df$classes <- ifelse(test.df$Model.Scores.1>roc.specs.1.test$threshold,1,0);

# Rough confusion matrix using counts;
table(test.df$PersonalLoan, test.df$classes)
#Proper confusion matrix using rates
t <- table(test.df$PersonalLoan, test.df$classes);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

##############################
#Model 2 Predictive Accuracy##
##############################
model.2.test <- predict(model.2, newdata = test.df, type = 'response')
summary(model.2.test)

roc.2.test <- roc(response=test.df$PersonalLoan, predictor=model.2.test)
print(roc.2.test)
plot(roc.2.test)

# Compute AUC
auc.2.test <- auc(roc.2.test);

#> auc.1.test
#Area under the curve is 0.9652

#Find threshold value of ROC Curve
roc.specs.2.test <- coords(roc=roc.2.test,x=c('best'),
                           input=c('threshold','specificity','sensitivity'),
                           ret=c('threshold','specificity','sensitivity'),
                           as.list=TRUE)

#Assign classes
test.df$Model.Scores.2 <- model.2.test
test.df$classes <- ifelse(test.df$Model.Scores.2>roc.specs.2.test$threshold,1,0);

# Rough confusion matrix using counts;
table(test.df$PersonalLoan, test.df$classes)
#Proper confusion matrix using rates
t <- table(test.df$PersonalLoan, test.df$classes);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r



##############################
#Model 3 Predictive Accuracy##
##############################
#Family Bins
breaks <- c(1,2,4)
tags <- c(("1-2"), ("3-4"))
test.new$Family.Bins <- cut(test.clean.copy$Family,
                             breaks = breaks,
                             include.lowest = TRUE,
                             right = FALSE,
                             labels = tags)
response.Family.New.Bins <- aggregate(test.new$PersonalLoan,
                                      by=list(Family.New.Bins=test.new$Family.Bins),
                                      FUN=mean)
barplot(height=response.Family.New.Bins$x,names.arg=response.Family.New.Bins$Family.New.Bins,
        xlab='Family Bins',ylab='Response Rate',las=2,cex.names=0.75)

#Income Bins
breaks <- c(0,63,82,92,114,163,224)
tags <- c(("8-63"), ("64-82"), ("83-92"), ("93-114"), ("115-163"), ("164-224"))
test.new$Income.Bins <- cut(test.clean.copy$Income,
                             breaks = breaks,
                             include.lowest = TRUE,
                             right = FALSE,
                             labels = tags)
response.Income.New.Bins <- aggregate(test.new$PersonalLoan,
                                      by=list(Income.New.Bins=test.new$Income.Bins),
                                      FUN=mean)
barplot(height=response.Income.New.Bins$x,names.arg=response.Income.New.Bins$Income.New.Bins,
        xlab='Family Bins',ylab='Response Rate',las=2,cex.names=0.75)

#CCAvg Bins
breaks <- c(0,2.5,2.9,10)
tags <- c(("0-2.5"), ("2.6-2.9"), ("3-10"))
test.new$CCAvg.Bins <- cut(test.clean.copy$CCAvg,
                            breaks = breaks,
                            include.lowest = TRUE,
                            right = FALSE,
                            labels = tags)
response.CCAvg.New.Bins <- aggregate(test.new$PersonalLoan,
                                     by=list(CCAvg.New.Bins=test.new$CCAvg.Bins),
                                     FUN=mean)
barplot(height=response.CCAvg.New.Bins$x,names.arg=response.CCAvg.New.Bins$CCAvg.New.Bins,
        xlab='Family Bins',ylab='Response Rate',las=2,cex.names=0.75)

#Education Bins
breaks <- c(1,2,3)
tags <- c(("1"), ("2-3"))
test.new$Education.Bins <- cut(test.clean.copy$Education,
                                breaks = breaks,
                                include.lowest = TRUE,
                                right = FALSE,
                                labels = tags)
response.Education.New.Bins <- aggregate(test.new$PersonalLoan,
                                         by=list(Education.New.Bins=test.new$Education.Bins),
                                         FUN=mean)
barplot(height=response.Education.New.Bins$x,names.arg=response.Education.New.Bins$Education.New.Bins,
        xlab='Family Bins',ylab='Response Rate',las=2,cex.names=0.75)

#Mortgage Bins
breaks <- c(0,290,617)
tags <- c(("0-290"), ("291-617"))
test.new$Mortgage.Bins <- cut(test.clean.copy$Mortgage,
                               breaks = breaks,
                               include.lowest = TRUE,
                               right = FALSE,
                               labels = tags)
response.Mortgage.New.Bins <- aggregate(test.new$PersonalLoan,
                                        by=list(Mortgage.New.Bins=test.new$Mortgage.Bins),
                                        FUN=mean)
barplot(height=response.Mortgage.New.Bins$x,names.arg=response.Mortgage.New.Bins$Mortgage.New.Bins,
        xlab='Family Bins',ylab='Response Rate',las=2,cex.names=0.75)

drop.list <- c('Income', 'Family', 'CCAvg', 'Education', 'Mortgage')
test.new <-test.new[,!(names(test.new) %in% drop.list)]
str(test.new)

model.3.test <- predict(model.3, newdata = test.new, type = 'response')
summary(model.3.test)

roc.3.test <- roc(response=test.new$PersonalLoan, predictor=model.3.test)
print(roc.3.test)
plot(roc.3.test)

# Compute AUC
auc.3.test <- auc(roc.3.test);

#> auc.3.test
#Area under the curve is 0.9639

#Find threshold value of ROC Curve
roc.specs.3.test <- coords(roc=roc.3.test,x=c('best'),
                           input=c('threshold','specificity','sensitivity'),
                           ret=c('threshold','specificity','sensitivity'),
                           as.list=TRUE)

#Assign classes
test.df$Model.Scores.3 <- model.3.test
test.df$classes <- ifelse(test.df$Model.Scores.3>roc.specs.3.test$threshold,1,0);

# Rough confusion matrix using counts;
table(test.df$PersonalLoan, test.df$classes)
#Proper confusion matrix using rates
t <- table(test.df$PersonalLoan, test.df$classes);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

Model <- c('Model 1', 'Model 2', 'Model 3')
AUC <- c(.9551, .9652, .9639)
True.Positive <- c(.8456, .9117, .9227)
True.Negative <- c(.9585, .9227, .9431)
outsample.df <- data.frame(Model, AUC, True.Positive, True.Negative)
outsample.df