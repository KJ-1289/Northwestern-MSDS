################################
# Section 1: Environment Setup #
################################

## Packages ##
library(car)
library(caret)
library(class)
library(corrplot)
library(corrr)
library(data.table)
library(e1071)
library(FactoMineR)
library(factoextra)
library(ggfortify)
library(ggplot2)
library(glmnet)
library(gridExtra)
library(lattice)
library(MASS)
library(Matrix)
library(mlbench)
library(mltools)
library(naivebayes)
library(OneR)
library(pROC)
library(randomForest)
library(rattle)
library(readr)
library(rpart)
library(rsample)
library(recipes)
library(stargazer)
library(stringr)
library(tidyquant)
library(tidyverse)
library(woeBinning)
library(xgboost)

#Color Palette (Bright Red #FF0000, Red #C93312, Dark Red #972D15, Light Blue #85D4E3,
#Dark Blue #046C9A, Lavender #9986A5, Gold #CCBA72, Light Gray #D9D0D3 Dark Gray #8D8680,
#Black #0F0D0E)

options(scipen = 999, digits = 4)
setwd("C:/Users/KJohn/OneDrive/Desktop/Projects/Open/Credit Capstone")
credit <- read.csv("credit_card_default.csv")
df <- data.frame(credit)
head(df)

set.seed(123)

#################################
# Section 2: Data Quality Check #
#################################

str(df)
drop <- c("X", "ID")
credit.df <- df[,!names(df) %in% drop]
summary(credit.df)

# Change incorrect values for categorical variables
credit.df %>% group_by(MARRIAGE) %>% summarise()
credit.df$MARRIAGE[credit.df$MARRIAGE == 0] <- 3 #Change unverified marriage to Other
credit.df$MARRIAGE <- as.factor(credit.df$MARRIAGE) #Change marriage to factor
levels(credit.df$MARRIAGE) <- c("Married","Single", "Other")

credit.df %>% group_by(EDUCATION) %>% summarise()
credit.df$EDUCATION[credit.df$EDUCATION == 0] <- 4 #Change unverified education to Other
credit.df$EDUCATION[credit.df$EDUCATION == 5] <- 4
credit.df$EDUCATION[credit.df$EDUCATION == 6] <- 4
credit.df$EDUCATION <- as.factor(credit.df$EDUCATION) #Change education to factor
levels(credit.df$EDUCATION) <- c("Grad School", "University", "High School", "Other")

credit.df$SEX <- as.factor(credit.df$SEX) #Change sex to factor
levels(credit.df$SEX) <- c("Male", "Female")

#Change negative PAY_n values to 0
credit.df$PAY_0[credit.df$PAY_0 == -2] <-  0
credit.df$PAY_0[credit.df$PAY_0 == -1] <-  0
credit.df$PAY_2[credit.df$PAY_2 == -2] <-  0
credit.df$PAY_2[credit.df$PAY_2 == -1] <-  0
credit.df$PAY_3[credit.df$PAY_3 == -2] <-  0
credit.df$PAY_3[credit.df$PAY_3 == -1] <-  0
credit.df$PAY_4[credit.df$PAY_4 == -2] <-  0
credit.df$PAY_4[credit.df$PAY_4 == -1] <-  0
credit.df$PAY_5[credit.df$PAY_5 == -2] <-  0
credit.df$PAY_5[credit.df$PAY_5 == -1] <-  0
credit.df$PAY_6[credit.df$PAY_6 == -2] <-  0
credit.df$PAY_6[credit.df$PAY_6 == -1] <-  0

#credit.df$PAY_0 <- as.factor(credit.df$PAY_0)
#credit.df$PAY_2 <- as.factor(credit.df$PAY_2)
#credit.df$PAY_3 <- as.factor(credit.df$PAY_3)
#credit.df$PAY_4 <- as.factor(credit.df$PAY_4)
#credit.df$PAY_5 <- as.factor(credit.df$PAY_5)
#credit.df$PAY_6 <- as.factor(credit.df$PAY_6)

# Change negative values in bill amount
bill.df <- credit.df[13:18]
bill.df[bill.df < 0] <- 0 
drop <- c("BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6")
credit.df <- credit.df[,!names(credit.df) %in% drop]
credit.df <- cbind(credit.df, bill.df)

# Change column names to easier to follow format
names(credit.df)[names(credit.df)=="PAY_0"] <- "PAY_1"
names(credit.df)[names(credit.df)=="BILL_AMT1"] <- "BILL_AMT0"
names(credit.df)[names(credit.df)=="BILL_AMT2"] <- "BILL_AMT1"
names(credit.df)[names(credit.df)=="BILL_AMT3"] <- "BILL_AMT2"
names(credit.df)[names(credit.df)=="BILL_AMT4"] <- "BILL_AMT3"
names(credit.df)[names(credit.df)=="BILL_AMT5"] <- "BILL_AMT4"
names(credit.df)[names(credit.df)=="BILL_AMT6"] <- "BILL_AMT5"
str(credit.df)


##################################
# Section 2: Feature Engineering #
##################################

#Utilization Rate = Bill Amt / Limit Amt, [0,100]
credit.df$UTL0 <- round((credit.df$BILL_AMT0/credit.df$LIMIT_BAL) * 100, 2)
credit.df$UTL1 <- round((credit.df$BILL_AMT1/credit.df$LIMIT_BAL) * 100, 2)
credit.df$UTL2 <- round((credit.df$BILL_AMT2/credit.df$LIMIT_BAL) * 100, 2)
credit.df$UTL3 <- round((credit.df$BILL_AMT3/credit.df$LIMIT_BAL) * 100, 2)
credit.df$UTL4 <- round((credit.df$BILL_AMT4/credit.df$LIMIT_BAL) * 100, 2)
credit.df$UTL5 <- round((credit.df$BILL_AMT5/credit.df$LIMIT_BAL) * 100, 2)
# Scale 0 to 100
credit.df$UTL0[credit.df$UTL0 > 100] <- 100
credit.df$UTL1[credit.df$UTL1 > 100] <- 100
credit.df$UTL2[credit.df$UTL2 > 100] <- 100
credit.df$UTL3[credit.df$UTL3 > 100] <- 100
credit.df$UTL4[credit.df$UTL4 > 100] <- 100
credit.df$UTL5[credit.df$UTL5 > 100] <- 100

#Pay Ratio = Pay Amt / Bill Amt [0,100]
credit.df$PAY_RAT1 <- round((credit.df$PAY_AMT1/credit.df$BILL_AMT1) * 100, 2)
credit.df$PAY_RAT2 <- round((credit.df$PAY_AMT2/credit.df$BILL_AMT2) * 100, 2)
credit.df$PAY_RAT3 <- round((credit.df$PAY_AMT3/credit.df$BILL_AMT3) * 100, 2)
credit.df$PAY_RAT4 <- round((credit.df$PAY_AMT4/credit.df$BILL_AMT4) * 100, 2)
credit.df$PAY_RAT5 <- round((credit.df$PAY_AMT5/credit.df$BILL_AMT5) * 100, 2)
# Scale 0 to 100
credit.df$PAY_RAT1[credit.df$PAY_RAT1 > 100] <- 100
credit.df$PAY_RAT2[credit.df$PAY_RAT2 > 100] <- 100
credit.df$PAY_RAT3[credit.df$PAY_RAT3 > 100] <- 100
credit.df$PAY_RAT4[credit.df$PAY_RAT4 > 100] <- 100
credit.df$PAY_RAT5[credit.df$PAY_RAT5 > 100] <- 100
#Pay Ratios for a 0 balance will be scored as 100
credit.df[is.na(credit.df)] <- 100

#Bill Change over Previous Month
credit.df$BILL_CHANGE_1M <- round((credit.df$BILL_AMT0/credit.df$BILL_AMT1), 2)
credit.df$BILL_CHANGE_1M[is.na(credit.df$BILL_CHANGE_1M)] <- 0
credit.df$BILL_CHANGE_1M[credit.df$BILL_CHANGE_1M=="Inf"] <- 100

#Bill Change over Simple Moving Average
credit.df$BILL_SMA <- round((credit.df$BILL_AMT1+credit.df$BILL_AMT2+credit.df$BILL_AMT3+
                               credit.df$BILL_AMT4+credit.df$BILL_AMT5)/5, 2)
credit.df$BILL_CHANGE_SMA <- round(credit.df$BILL_AMT0/credit.df$BILL_SMA, 2)
credit.df$BILL_CHANGE_SMA[is.na(credit.df$BILL_CHANGE_SMA)] <- 0
credit.df$BILL_CHANGE_SMA[credit.df$BILL_CHANGE_SMA=="Inf"] <- 100

#Bill Change over Weighted Moving Average
credit.df$BILL_WMA <- round((5*credit.df$BILL_AMT1+4*credit.df$BILL_AMT2+
                               3*credit.df$BILL_AMT3+2*credit.df$BILL_AMT4+
                               credit.df$BILL_AMT5)/(5+4+3+2+1), 2)
credit.df$BILL_CHANGE_WMA <- round(credit.df$BILL_AMT0/credit.df$BILL_WMA, 2)
credit.df$BILL_CHANGE_WMA[is.na(credit.df$BILL_CHANGE_WMA)] <- 0
credit.df$BILL_CHANGE_WMA[credit.df$BILL_CHANGE_WMA=="Inf"] <- 100

credit.df$BILL_SMA <- round((credit.df$BILL_AMT0+credit.df$BILL_AMT1+
                               credit.df$BILL_AMT2+credit.df$BILL_AMT3+
                               credit.df$BILL_AMT4+credit.df$BILL_AMT5)/6, 2)
credit.df$BILL_WMA <- round((6*credit.df$BILL_AMT0+5*credit.df$BILL_AMT1+4*credit.df$BILL_AMT2+
                               3*credit.df$BILL_AMT3+2*credit.df$BILL_AMT4+
                               credit.df$BILL_AMT5)/(6+5+4+3+2+1), 2)

#Payment Moving Averages
credit.df$PAY_AMT_SMA <- round((credit.df$PAY_AMT1+credit.df$PAY_AMT2+credit.df$PAY_AMT3+
                                  credit.df$PAY_AMT4+credit.df$PAY_AMT5)/5, 2)
credit.df$PAY_AMT_WMA <- round((5*credit.df$PAY_AMT1+4*credit.df$PAY_AMT2+
                                  3*credit.df$PAY_AMT3+2*credit.df$PAY_AMT4+
                                  credit.df$PAY_AMT5)/(5+4+3+2+1), 2)

#Utilization Rate Change from Previous Month
credit.df$UTL_CHANGE_1M <- credit.df$UTL0-credit.df$UTL1

#Utilization Rate Change from Moving Averages
credit.df$UTL_SMA <- round((credit.df$UTL1+credit.df$UTL2+credit.df$UTL3+credit.df$UTL4+
                              credit.df$UTL5)/5, 2)
credit.df$UTL_CHANGE_SMA <- credit.df$UTL0-credit.df$UTL_SMA
credit.df$UTL_WMA <- round((5*credit.df$UTL1+4*credit.df$UTL2+3*credit.df$UTL3+
                              2*credit.df$UTL4+credit.df$UTL5)/(5+4+3+2+1), 2)
credit.df$UTL_CHANGE_WMA <- credit.df$UTL0-credit.df$UTL_WMA
credit.df$BILL_CHANGE_WMA[is.na(credit.df$BILL_CHANGE_WMA)] <- 0
credit.df$BILL_CHANGE_WMA[credit.df$BILL_CHANGE_WMA=="Inf"] <- 100

credit.df$UTL_SMA <- round((credit.df$UTL0+credit.df$UTL1+credit.df$UTL2+
                              credit.df$UTL3+credit.df$UTL4+credit.df$UTL5)/5, 2)
credit.df$UTL_WMA <- round((6*credit.df$UTL0+5*credit.df$UTL1+4*credit.df$UTL2+3*credit.df$UTL3+
                              2*credit.df$UTL4+credit.df$UTL5)/(6+5+4+3+2+1), 2)

#Bill Amount times Pay Ratio Moving Averages
credit.df$PAY_RAT_SMA <- round((credit.df$PAY_RAT1+credit.df$PAY_RAT2+credit.df$PAY_RAT3+
                                  credit.df$PAY_RAT4+credit.df$PAY_RAT5)/5, 2)
credit.df$PAY_RAT_WMA <- round((5*credit.df$PAY_RAT1+4*credit.df$PAY_RAT2+
                                  3*credit.df$PAY_RAT3+2*credit.df$PAY_RAT4+
                                  credit.df$PAY_RAT5)/(5+4+3+2+1), 2)

#credit.df$PAY_RAT_PM[is.na(credit.df$PAY_RAT_PM)] <- 0
credit.df$PAY_RAT_1M <- credit.df$PAY_RAT1 - credit.df$PAY_RAT2

small.df <- subset(credit.df, select=c("BILL_AMT1", "BILL_AMT2", "BILL_AMT3",
                                      "BILL_AMT4", "BILL_AMT5", "PAY_AMT1",
                                      "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5",
                                      "PAY_AMT6", "UTL1", "UTL2", "UTL3", "UTL4", "UTL5",
                                      "PAY_RAT1", "PAY_RAT2", "PAY_RAT3", "PAY_RAT4", 
                                      "PAY_RAT5"))
drop <- c(names(small.df))
credit.df <- credit.df[,!names(credit.df) %in% drop]

summary(credit.df)

##################
# Split Datasets #
##################

train.df <- subset(credit.df, train==1)
test.df <- subset(credit.df, test==1)
validate.df <- subset(credit.df, validate==1)

drop <- c("u", "train", "test", "validate", "data.group")
train.df <- train.df[,!names(train.df) %in% drop]
test.df <- test.df[,!names(test.df) %in% drop]
validate.df <- validate.df[,!names(validate.df) %in% drop]

str(train.df)
str(test.df)
str(validate.df)

#################################
# Section 3: Variable Based EDA #
#################################

#Categorical Variables
woe.bal <- woe.binning(df=train.df,target.var=c('DEFAULT'),
                       pred.var=c('LIMIT_BAL'))
woe.binning.table(woe.bal)
train.df <- woe.binning.deploy(df=train.df,binning=woe.bal)
woe.age <- woe.binning(df=train.df,target.var=c('DEFAULT'),
                       pred.var=c('AGE'))
woe.binning.table(woe.age)
train.df <- woe.binning.deploy(df=train.df,binning=woe.age)

#Drop unbinned variables
drop <- c("AGE", "LIMIT_BAL")
train.df <- train.df[,!names(train.df) %in% drop]

#Color Palette (Bright Red #FF0000, Red #C93312, Dark Red #972D15, Light Blue #85D4E3,
#Dark Blue #046C9A, Lavender #9986A5, Gold #CCBA72, Light Gray #D9D0D3 Dark Gray #8D8680,
#Black #0F0D0E)

edu.bar <- ggplot(train.df, aes(x = EDUCATION, fill = factor(DEFAULT))) +
  geom_bar() +
  scale_fill_manual(values=c("#046C9A","#FF0000")) +
  labs(x = 'Education') +
  theme_light()
mar.bar <- ggplot(train.df, aes(x = MARRIAGE, fill = factor(DEFAULT))) +
  geom_bar() +
  scale_fill_manual(values=c("#046C9A","#FF0000")) +
  labs(x = 'Marriage') +
  theme_light()
sex.bar <- ggplot(train.df, aes(x = SEX, fill = factor(DEFAULT))) +
  geom_bar() +
  scale_fill_manual(values=c("#046C9A","#FF0000")) +
  labs(x = 'Sex') +
  theme_light()
age.bar <- ggplot(train.df, aes(x = AGE.binned, fill = factor(DEFAULT))) +
  geom_bar() +
  scale_fill_manual(values=c("#046C9A","#FF0000")) +
  labs(x = 'Age') +
  theme_light()
lim.bar <- ggplot(train.df, aes(x = LIMIT_BAL.binned, fill = factor(DEFAULT))) +
  geom_bar() +
  scale_fill_manual(values=c("#046C9A","#FF0000")) +
  labs(x = 'Limit Balance') +
  theme_light()
grid.arrange(edu.bar, mar.bar, sex.bar, age.bar, lim.bar, ncol=2)

# Numerical Variables
woe.pay.sma <- woe.binning(df=train.df,target.var=c('DEFAULT'),
                           pred.var=c('PAY_RAT_SMA'))
woe.binning.table(woe.pay.sma)
woe.pay.wma <- woe.binning(df=train.df,target.var=c('DEFAULT'),
                           pred.var=c('PAY_RAT_WMA'))
woe.binning.table(woe.pay.wma)
woe.paya.sma <- woe.binning(df=train.df,target.var=c('DEFAULT'),
                           pred.var=c('PAY_AMT_SMA'))
woe.binning.table(woe.paya.sma)
woe.paya.wma <- woe.binning(df=train.df,target.var=c('DEFAULT'),
                           pred.var=c('PAY_AMT_WMA'))
woe.binning.table(woe.paya.wma)

woe.bill.sma <- woe.binning(df=train.df,target.var=c('DEFAULT'),
                            pred.var=c('BILL_SMA'))
woe.binning.table(woe.bill.sma)
woe.bill.wma <- woe.binning(df=train.df,target.var=c('DEFAULT'),
                            pred.var=c('BILL_WMA'))
woe.binning.table(woe.bill.wma)
woe.utl.sma <- woe.binning(df=train.df,target.var=c('DEFAULT'),
                            pred.var=c('UTL_SMA'))
woe.binning.table(woe.utl.sma)
woe.utl.wma <- woe.binning(df=train.df,target.var=c('DEFAULT'),
                            pred.var=c('UTL_WMA'))
woe.binning.table(woe.utl.wma)
#write.csv(train.df, "trainingset.csv")

pay.sma.box <- ggplot(train.df, aes(x=factor(DEFAULT), y=PAY_AMT_SMA, fill=factor(DEFAULT))) + 
  geom_boxplot() +
  coord_cartesian(ylim=c(0,20000)) +
  scale_fill_manual(values = c('#046C9A', '#FF0000')) +
  labs(x = 'Default', y = 'PAY_SMA', title = 'PAY_SMA') +
  theme_light()
pay.wma.box <- ggplot(train.df, aes(x=factor(DEFAULT), y=PAY_AMT_WMA, fill=factor(DEFAULT))) + 
  geom_boxplot() +
  coord_cartesian(ylim=c(0,20000)) +
  scale_fill_manual(values = c('#046C9A', '#FF0000')) +
  labs(x = 'Default', y = 'PAY_WMA', title = 'PAY_WMA') +
  theme_light()
bill.sma.box <- ggplot(train.df, aes(x=factor(DEFAULT), y=BILL_SMA, fill=factor(DEFAULT))) + 
  geom_boxplot() +
  coord_cartesian(ylim=c(0,200000)) +
  scale_fill_manual(values = c('#046C9A', '#FF0000')) +
  labs(x = 'Default', y = 'BILL_SMA', title = 'BILL_SMA') +
  theme_light()
bill.wma.box <- ggplot(train.df, aes(x=factor(DEFAULT), y=BILL_WMA, fill=factor(DEFAULT))) + 
  geom_boxplot() +
  coord_cartesian(ylim=c(0,200000)) +
  scale_fill_manual(values = c('#046C9A', '#FF0000')) +
  labs(x = 'Default', y = 'BILL_WMA', title = 'BILL_WMA') +
  theme_light()
utl.sma.box <- ggplot(train.df, aes(x=factor(DEFAULT), y=UTL_SMA, fill=factor(DEFAULT))) + 
  geom_boxplot() +
  coord_cartesian(ylim=c(0,100)) +
  scale_fill_manual(values = c('#046C9A', '#FF0000')) +
  labs(x = 'Default', y = 'UTL_SMA', title = 'UTL_SMA') +
  theme_light()
utl.wma.box <- ggplot(train.df, aes(x=factor(DEFAULT), y=UTL_WMA, fill=factor(DEFAULT))) + 
  geom_boxplot() +
  coord_cartesian(ylim=c(0,100)) +
  scale_fill_manual(values = c('#046C9A', '#FF0000')) +
  labs(x = 'Default', y = 'UTL_WMA', title = 'UTL_WMA') +
  theme_light()
grid.arrange(pay.sma.box, pay.wma.box, bill.sma.box, bill.wma.box, 
             utl.sma.box, utl.wma.box)


#Principal Components Analysis
pay.df <- subset(train.df, select=c("PAY_1", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6"))

cor <- cor(pay.df)
col <- colorRampPalette(c("#85D4E3", "white","#FF0000"))
corrplot(cor, method="color", col=col(125),  order="alphabet", addCoef.col = "black", 
         number.digits = 2, tl.col="black", tl.srt=45, diag=TRUE, 
         number.cex= 20/ncol(small.df), addCoefasPercent = TRUE)

pay.pca <- prcomp(pay.df, scale. = T)
summary(pay.pca)

print(pay.pca)
summary(pay.pca)
eig.val <- get_eigenvalue(pay.pca)
eig.val
fviz_eig(pay.pca, addlabels = TRUE, ylim = c(0, 50), barfill="#046C9A", 
         barcolor="#046C9A", linecolor="black")

var <- get_pca_var(pay.pca)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
# Coordinates of variables
head(var$coord, 4)

corrplot(var$cos2[,1:4], is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pay.pca, choice = "var", axes = 1:2, color="#046C9A")

# Color by cos2 values: quality on the factor map
fviz_pca_var(pay.pca, col.var = "cos2",
             gradient.cols = c("#FF0000", "white", "#046C9A"), 
             repel = TRUE)

# Contributions of variables
dim.1.con <- fviz_contrib(pay.pca, choice = "var", axes = 1, top = 10, fill="#046C9A")
dim.1.cos2 <- fviz_cos2(pay.pca, choice = "var", axes = 1, top = 10, fill="#FF0000")
dim.2.con <- fviz_contrib(pay.pca, choice = "var", axes = 2, top = 10, fill="#046C9A")
dim.2.cos2 <- fviz_cos2(pay.pca, choice = "var", axes = 2, top = 10, fill="#FF0000")
grid.arrange(dim.1.con, dim.1.cos2, dim.2.con, dim.2.cos2, ncol=2)

fviz.con <- fviz_pca_var(pay.pca, col.var = "contrib",
                         gradient.cols = c("#FF0000", "white", "#046C9A"), repel=TRUE)
fviz.cos2 <- fviz_pca_var(pay.pca, col.var = "cos2",
                          gradient.cols = c("#FF0000", "white", "#046C9A"), 
                          repel = TRUE)
grid.arrange(fviz.con, fviz.cos2, ncol=2)

pca.dims <- data.frame(pay.pca$x[,1:2])
colnames(pca.dims) <- c("Dim1", "Dim2")

train.df <- cbind(train.df, pca.dims)
drop <- c(names(pay.df))
train.df <- train.df[,!names(train.df) %in% drop]
str(train.df)

#################################
# Section 4: Model Based EDA #
#################################

##################
# One Rule Model #
##################

train.df2 <- train.df %>% select(-"DEFAULT", everything())
data <- optbin(train.df2)
oner.model <- OneR(data, verbose=TRUE)
summary(oner.model)
predict <- predict(oner.model, train.df2)
eval_model(predict, train.df2)

prediction <- as.numeric(predict)
roc.1 <- roc(response=train.df2$DEFAULT, predictor=prediction)
roc.1
confusionMatrix(factor(train.df2$DEFAULT), predict)


#################
# Decision Tree #
#################

eda.dt<- rpart::rpart(formula = DEFAULT~.,data = train.df,
                      method = "class",
                      parms = list(split = "gini"),
                      control = rpart::rpart.control(
                        cp = -1,
                        maxcompete = 3,
                        minbucket = 1,
                        maxsurrogate = 3,
                        xval = 20,
                        maxdepth = 4
                        )
                      )

fancyRpartPlot(eda.dt)

# Variable importance plot
x <- head(eda.dt$variable.importance, 10)
dotplot(x,xlab='Variable Importance', main='Decision Tree',pch=15)

model.dt.train <- predict(eda.dt, newdata=train.df, type=c("vector"))
model.dt.train <- ifelse(model.dt.train==1, 0, 1)

roc.1 <- roc(response=train.df$DEFAULT, predictor=model.dt.train)
roc.1
confusionMatrix(factor(train.df$DEFAULT), factor(model.dt.train))

#################
# Random Forest #
#################

eda.rf <- randomForest(as.factor(DEFAULT) ~., data=train.df)
varImpPlot(eda.rf, sort=TRUE, pch=15, main="Random Forest")
eda.rf$importance

roc.1 <- roc(response=train.df$DEFAULT, predictor=as.numeric(eda.rf$predicted))
roc.1
confusionMatrix(factor(train.df$DEFAULT), eda.rf$predicted)

##########################################
# Logistic Regression Backward Selection #
##########################################

upper.lm <- glm(DEFAULT ~ .,data=train.df)
backward.glm <- stepAIC(object=upper.lm,direction=c('backward'))
summary(backward.glm)
stargazer(backward.glm, type="html", report = ("vc*p"), out = "BackwardLogisticEDA.doc",
          title="Computational EDA: Logistic Regression with Backward Selection")

roc.1 <- roc(response=train.df$DEFAULT, predictor=backward.glm$fitted.values)
roc.1
roc.specs.1 <- coords(roc=roc.1,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE)
bwlr.class <- ifelse(backward.glm$fitted.values>roc.specs.1$threshold,1,0)
confusionMatrix(factor(train.df$DEFAULT), factor(bwlr.class))


################################
# Section 5: Model Development #
################################

model.df <- subset(train.df, select=c("DEFAULT", "Dim1", "Dim2", "BILL_AMT0", "BILL_CHANGE_1M",
                                      "PAY_AMT_SMA", "UTL_CHANGE_1M", "UTL_CHANGE_WMA",
                                      "UTL_SMA","PAY_RAT_1M", "PAY_RAT_SMA", "EDUCATION",
                                      "MARRIAGE", "SEX", "LIMIT_BAL.binned", "AGE.binned"))

test.df <- woe.binning.deploy(df=test.df,binning=woe.bal)
test.df <- woe.binning.deploy(df=test.df,binning=woe.age)
pay.test <- subset(test.df, select=c("PAY_1", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6"))
pca.test <- predict(pay.pca, newdata=pay.test)
pca.test.df <- as.data.frame(pca.test[,1:2])
colnames(pca.test.df) <- c("Dim1", "Dim2")
test.df <- cbind(test.df, pca.test.df)

validate.df <- woe.binning.deploy(df=validate.df,binning=woe.bal)
validate.df <- woe.binning.deploy(df=validate.df,binning=woe.age)
pay.validate <- subset(validate.df, select=c("PAY_1", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6"))
pca.validate <- predict(pay.pca, newdata=pay.validate)
pca.validate.df <- as.data.frame(pca.validate[,1:2])
colnames(pca.validate.df) <- c("Dim1", "Dim2")
validate.df <- cbind(validate.df, pca.validate.df)

model.test <- subset(test.df, select=c("DEFAULT", "Dim1", "Dim2", "BILL_AMT0", "BILL_CHANGE_1M",
                                       "PAY_AMT_SMA", "UTL_CHANGE_1M", "UTL_CHANGE_WMA",
                                       "UTL_SMA","PAY_RAT_1M", "PAY_RAT_SMA", "EDUCATION",
                                       "MARRIAGE", "SEX", "LIMIT_BAL.binned", "AGE.binned"))
model.validate <- subset(validate.df, select=c("DEFAULT", "Dim1", "Dim2", "BILL_AMT0", "BILL_CHANGE_1M",
                                               "PAY_AMT_SMA", "UTL_CHANGE_1M", "UTL_CHANGE_WMA",
                                               "UTL_SMA","PAY_RAT_1M", "PAY_RAT_SMA", "EDUCATION",
                                               "MARRIAGE", "SEX", "LIMIT_BAL.binned", "AGE.binned"))
str(model.df)
str(model.test)
str(model.validate)

##########################
# Model 1: Random Forest #
##########################

model1.rf <- randomForest(as.factor(DEFAULT) ~., data=model.df)
varImpPlot(model1.rf, sort=TRUE, pch=15, main="Random Forest")
model1.rf$importance

roc.1 <- roc(response=model.df$DEFAULT, predictor=as.numeric(model1.rf$predicted))
roc.1
confusionMatrix(factor(model.df$DEFAULT), model1.rf$predicted)

model1.test <- predict(model1.rf, newdata=test.df)
roc.1.test <- roc(response=test.df$DEFAULT, predictor=as.numeric(model1.test))
roc.1.test
confusionMatrix(factor(model.test$DEFAULT), model1.test)

plot(roc.1)
plot(roc.1.test)

##############################
# Model 2: Gradient Boosting #
##############################

xgb.df <- model.df
sparse_matrix <- sparse.model.matrix(DEFAULT ~ ., data = xgb.df)[,-1]
X_train_mat <- xgb.DMatrix(sparse_matrix, label = model.df$DEFAULT)

xgb.test <- model.test
sparse_matrix_test <- sparse.model.matrix(DEFAULT ~ ., data = xgb.test)[,-1]
X_test_mat <- xgb.DMatrix(sparse_matrix_test, label = model.test$DEFAULT)

#XGboost parameters
xgb_params = list(
  objective = "binary:logistic",
  eta = .001,
  max.depth = 6,
  gamma = 1,
  eval_metric = "auc"
)

#Fit the model with the parameters specified above
model2.xgb <- xgboost(params = xgb_params,
                   data = X_train_mat,
                   label = as.factor(model.df$DEFAULT),
                   nrounds = 600,
                   verbose = TRUE,                                    
                   print.every.n = 1,
                   early.stop.rounds = 10
)

xgb.var.importance <- xgb.importance(feature_names=colnames(X_train_mat),model=model2.xgb)
xgb.plot.importance(xgb.var.importance, rel_to_first=TRUE, xlab='Relative Importance',
                    main='Model 2: XGBoost')

xgb.score <- predict(model2.xgb, newdata=X_train_mat)
roc.2 <- roc(response=model.df$DEFAULT, predictor=xgb.score)
roc.2
roc.specs.2 <- coords(roc=roc.2,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE
)
xgb.class <- ifelse(xgb.score>roc.specs.2$threshold,1,0)
confusionMatrix(factor(model.df$DEFAULT), factor(xgb.class))

model2.test <- predict(model2.xgb, newdata=X_test_mat)
roc.2.test <- roc(response=model.test$DEFAULT, predictor=model2.test)
roc.2.test
roc.specs.test.2 <- coords(roc=roc.2.test,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE
)
xgb.class.test <- ifelse(model2.test>roc.specs.test.2$threshold,1,0)
confusionMatrix(factor(model.test$DEFAULT), factor(xgb.class.test))

plot(roc.2)
plot(roc.2.test)

########################
# Model 3: Naive Bayes #
########################

predictor.df <- model.df[,-1]
model3.nb <- naive_bayes(x=predictor.df, y=factor(model.df$DEFAULT), laplace=3)

nb.predicted.class <- predict(model3.nb)
roc.3 <- roc(response=model.df$DEFAULT, predictor=as.numeric(nb.predicted.class))
roc.3
confusionMatrix(factor(model.df$DEFAULT), nb.predicted.class)

# Predict the class;
model3.test <- predict(model3.nb, newdata = model.test[,-1])
roc.3.test <- roc(response=model.test$DEFAULT, predictor=as.numeric(model3.test))
roc.3.test
confusionMatrix(factor(model.test$DEFAULT), model3.test)

#################################
# Final Model: Lasso Regression #
#################################

xgb.validate <- model.validate
sparse_matrix_validate <- sparse.model.matrix(DEFAULT ~ ., data = xgb.validate)[,-1]

X_train <- sparse_matrix
Y_train <- model.df$DEFAULT

X_test <- sparse_matrix_test
Y_test <- model.test$DEFAULT

X_validate <- sparse_matrix_validate
Y_validate <- model.validate$DEFAULT

cv.lasso <- cv.glmnet(X_train, Y_train, alpha = 1, type.measure = "auc")
lambda_lasso <- cv.lasso$lambda.min

fit.lasso <- glmnet(X_train, Y_train, family="binomial", alpha=1, lambda=lambda_lasso)
model.lr.final <- fit.lasso %>% predict(X_train) %>% as.vector()

roc.final <- roc(response=model.df$DEFAULT, predictor=model.lr.final)
roc.final

#Training Results
roc.specs.lasso <- coords(roc=roc.final,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE)
lasso.class <- ifelse(model.lr.final>roc.specs.lasso$threshold,1,0)
confusionMatrix(factor(model.df$DEFAULT), factor(lasso.class))

#Test Results
model.lr.final.test <- fit.lasso %>% predict(X_test) %>% as.vector()
roc.final.test <- roc(response=model.test$DEFAULT, predictor=model.lr.final.test)
roc.final.test

roc.specs.final.test <- coords(roc=roc.final.test,x=c('best'),
                           input=c('threshold','specificity','sensitivity'),
                           ret=c('threshold','specificity','sensitivity'),
                           as.list=TRUE)
lasso.class.test <- ifelse(model.lr.final.test>roc.specs.final.test$threshold,1,0)
confusionMatrix(factor(model.test$DEFAULT), factor(lasso.class.test))

#Validate Results
model.lr.final.validate <- fit.lasso %>% predict(X_validate) %>% as.vector()
roc.final.validate <- roc(response=model.validate$DEFAULT, predictor=model.lr.final.validate)
roc.final.validate

roc.specs.final.validate <- coords(roc=roc.final.validate,x=c('best'),
                               input=c('threshold','specificity','sensitivity'),
                               ret=c('threshold','specificity','sensitivity'),
                               as.list=TRUE)
lasso.class.validate <- ifelse(model.lr.final.validate>roc.specs.final.validate$threshold,1,0)
confusionMatrix(factor(model.validate$DEFAULT), factor(lasso.class.validate))

sens.ci <- ci.se(roc.final.validate)
plot(sens.ci, type="shape", col="lightblue")

#KS Test
normalize <- function(x) {
  denom <- ifelse(max(x) - min(x) == 0, 1, max(x) - min(x))
  return ((x - min(x)) / denom)
}

train.score <- normalize(model.lr.final)
test.score <- normalize(model.lr.final.test)
validate.score <- normalize (model.lr.final.validate)

train.response <- model.df$DEFAULT
test.response <- model.test$DEFAULT
validate.response <- model.validate$DEFAULT

ks.train <- as.data.frame(cbind(train.score,train.response))
ks.test <- as.data.frame(cbind(test.score,test.response))
ks.validate <- as.data.frame(cbind(validate.score,validate.response))

decile.train <- quantile(ks.train$train.score,
                       probs=c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40,
                               0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80,
                               0.85, 0.90, 0.95))
decile.test <- quantile(ks.test$test.score,
                       probs=c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40,
                               0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80,
                               0.85, 0.90, 0.95));
decile.validate <- quantile(ks.validate$validate.score,
                       probs=c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40,
                               0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80,
                               0.85, 0.90, 0.95));

ks.train$model.decile <- cut(ks.train$train.score,breaks=c(0,decile.train,1),
                          labels=rev(c('01','02','03','04','05','06','07','08','09','10',
                                       '11', '12', '13', '14', '15', '16', '17', '18',
                                       '19', '20'))
)
ks.test$model.decile <- cut(ks.test$test.score,breaks=c(0,decile.test,1),
                          labels=rev(c('01','02','03','04','05','06','07','08','09','10',
                                       '11', '12', '13', '14', '15', '16', '17', '18',
                                       '19', '20'))
)
ks.validate$model.decile <- cut(ks.validate$validate.score,breaks=c(0,decile.validate,1),
                          labels=rev(c('01','02','03','04','05','06','07','08','09','10',
                                       '11', '12', '13', '14', '15', '16', '17', '18',
                                       '19', '20'))
);

aggregate(ks.train$train.score,by=list(Decile=ks.train$model.decile),FUN=min);
table(ks.train$model.decile)
table(ks.train$model.decile,ks.train$train.response)

aggregate(ks.test$test.score,by=list(Decile=ks.test$model.decile),FUN=min);
table(ks.test$model.decile)
table(ks.test$model.decile,ks.test$test.response)

aggregate(ks.validate$validate.score,by=list(Decile=ks.validate$model.decile),FUN=min);
table(ks.validate$model.decile)
table(ks.validate$model.decile,ks.validate$validate.response)

train.table <- as.data.frame(list(Y0=table(ks.train$model.decile,ks.train$train.response)[,1],
                               Y1=table(ks.train$model.decile,ks.train$train.response)[,2],
                               Decile=rev(c('01','02','03','04','05','06','07','08','09',
                                            '10', '11', '12', '13', '14', '15', '16', '17',
                                            '18', '19', '20'))
));
train.table <- train.table[order(train.table$Decile),]

test.table <- as.data.frame(list(Y0=table(ks.test$model.decile,ks.test$test.response)[,1],
                                  Y1=table(ks.test$model.decile,ks.test$test.response)[,2],
                                  Decile=rev(c('01','02','03','04','05','06','07','08','09',
                                               '10', '11', '12', '13', '14', '15', '16', '17',
                                               '18', '19', '20'))
));
test.table <- test.table[order(test.table$Decile),]

validate.table <- as.data.frame(list(Y0=table(ks.validate$model.decile,ks.validate$validate.response)[,1],
                                 Y1=table(ks.validate$model.decile,ks.validate$validate.response)[,2],
                                 Decile=rev(c('01','02','03','04','05','06','07','08','09',
                                              '10', '11', '12', '13', '14', '15', '16', '17',
                                              '18', '19', '20'))
));
validate.table <- validate.table[order(validate.table$Decile),]



train.table
test.table
validate.table
