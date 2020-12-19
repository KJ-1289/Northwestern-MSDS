##Assignment 7 Automated Variable Selection

setwd("C:/Users/KJohn/OneDrive/Desktop/Northwestern/MSDS 410 Data Modeling for Supervised Learning/Assignment 7 Automated Variable Selection")
ames.df <- read.csv("ames_housing_data.csv", header = TRUE, stringsAsFactors = FALSE)
str(ames.df)

ames.df$dropCondition <- ifelse(ames.df$BldgType!='1Fam','01: Not SFR',
                                ifelse(ames.df$SaleCondition!='Normal','02: Non-Normal Sale',
                                       ifelse(ames.df$Street!='Pave','03: Street Not Paved',
                                              ifelse(ames.df$YearBuilt <1950,'04: Built Pre-1950',
                                                     ifelse(ames.df$TotalBsmtSF <1,'05: No Basement',
                                                            ifelse(ames.df$GrLivArea <800,'06: LT 800 SqFt',
                                                                   ifelse(ames.df$Utilities!='AllPub','07: Not Public Utilities',
                                                                          '99: Eligible Sample')
                                                            ))))))

# Save the table
waterfall <- table(ames.df$dropCondition)

# Format the table as a column matrix for presentation;
as.matrix(waterfall,7,1)

# Eliminate all observations that are not part of the eligible sample population;
eligible.population <- subset(ames.df,dropCondition=='99: Eligible Sample')

# Check that all remaining observations are eligible;
table(eligible.population$dropCondition)

ames.eligible <- readRDS("Ames_eligible_sample.Rdata")
my.data <- data.frame(ames.eligible)
str(my.data)

## 1.2 Predictive Modeling Framework

# Set the seed on the random number generator so you get the same split every time that
# you run the code.
set.seed(123)
my.data$u <- runif(n=dim(my.data)[1],min=0,max=1)

# Define these two variables for later use
my.data$QualityIndex <- my.data$OverallQual*my.data$OverallCond
my.data$TotalSqftCalc <- my.data$BsmtFinSF1+my.data$BsmtFinSF2+my.data$GrLivArea

# Create train/test split
train.df <- subset(my.data, u<0.70)
test.df <- subset(my.data, u>=0.70)

# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(my.data)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)

Table <- c('Train', 'Test', 'Total')
dim.train <- dim(train.df)[1]
dim.test <- dim(test.df)[1]
dim.total <- dim.train + dim.test
dims <- c(dim.train, dim.test, dim.total)
pct.train <- dim.train/dim.total
pct.test <- dim.test/dim.total
pct.total <- dim.total/1469
pcts <- c(pct.train, pct.test, pct.total)
Table.df <- data.frame(Table, dims, pcts)
Table.df

## (2) Model Identification and In-Sample Fit
str(train.df)
train.clean <- subset(train.df, select =  c('SalePrice', 'QualityIndex', 'TotalSqftCalc',
                                  'SubClass', 'LotArea', 'TotalBsmtSF', 'GrLivArea', 
                                  'FullBath', 'HalfBath', 'BedroomAbvGr', 'KitchenAbvGr', 
                                  'TotRmsAbvGrd', 'FirstFlrSF', 'SecondFlrSF',
                                  'KitchenQual', 'Fireplaces', 'GarageCars', 'CentralAir'))
str(train.clean)
diagnose(train.clean)

#Define the upper model as the FULL model
upper.lm <- lm(SalePrice ~ .,data=train.clean);
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ 1,data=train.clean)
summary(lower.lm)

# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalSqftCalc,data=train.clean);
summary(sqft.lm)

# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it;
library(MASS)

# Call stepAIC() for variable selection
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=formula(upper.lm),lower=~1),
                      direction=c('forward'));
summary(forward.lm)
backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)
stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)

#Junk Model
junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea +
                TotalSqftCalc, data=train.df)
summary(junk.lm)

# Compute the VIF values
library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm), decreasing=TRUE)

aic.f <- AIC(forward.lm)
aic.b <- AIC(backward.lm)
aic.s <- AIC(stepwise.lm)
aic.j <- AIC(junk.lm)
bic.f <- BIC(forward.lm)
bic.b <- BIC(backward.lm)
bic.s <- BIC(stepwise.lm)
bic.j <- BIC(junk.lm)
mse.f <- mean((forward.lm$residuals)^2)
mse.b <- mean((backward.lm$residuals)^2)
mse.s <- mean((stepwise.lm$residuals)^2)
mse.j <- mean((junk.lm$residuals)^2)
mae.f <- mean((abs(forward.lm$residuals)))
mae.b <- mean((abs(backward.lm$residuals)))
mae.s <- mean((abs(stepwise.lm$residuals)))
mae.j <- mean((abs(junk.lm$residuals)))

Model <- c('Forward', 'Backward', 'Stepwise', 'Junk')
Adj.R2 <- c(.8863, .8863, .8863, .8431)
AIC <- c(aic.f, aic.b, aic.s, aic.j)
BIC <- c(bic.f, bic.b, bic.s, bic.j)
MSE <- c(mse.f, mse.b, mse.s, mse.j)
MAE <- c(mae.f, mae.b, mae.s, mae.j)
scores.df <- data.frame(Model, Adj.R2, AIC, BIC, MSE, MAE)
scores.df

## (3) Predictive Accuracy
test.clean <- subset(test.df, select =  c('SalePrice', 'QualityIndex', 'TotalSqftCalc',
                                            'SubClass', 'LotArea', 'TotalBsmtSF', 'GrLivArea', 
                                            'FullBath', 'HalfBath', 'BedroomAbvGr', 'KitchenAbvGr', 
                                            'TotRmsAbvGrd', 'FirstFlrSF', 'SecondFlrSF',
                                            'KitchenQual', 'Fireplaces', 'GarageCars', 'CentralAir'))

forward.test <- predict(forward.lm,newdata=test.clean)
backward.test <- predict(backward.lm,newdata=test.clean)
stepwise.test <- predict(stepwise.lm,newdata=test.clean)
junk.test <- predict(junk.lm, newdata=test.df)

mse.ft <- mean((test.clean$SalePrice - forward.test)^2)
mse.bt <- mean((test.clean$SalePrice - backward.test)^2)
mse.st <- mean((test.clean$SalePrice - stepwise.test)^2)
mse.jt <- mean((test.df$SalePrice - junk.test)^2)
mae.ft <- mean(abs(test.clean$SalePrice - forward.test))
mae.bt <- mean(abs(test.clean$SalePrice - backward.test))
mae.st <- mean(abs(test.clean$SalePrice - stepwise.test))
mae.jt <- mean(abs(test.clean$SalePrice - junk.test))
Model <- c('Forward', 'Backward', 'Stepwise', 'Junk')
MSE <- c(mse.ft, mse.bt, mse.st, mse.jt)
MAE <- c(mae.ft, mae.bt, mae.st, mae.jt)
pred.df <- data.frame(Model, MSE, MAE)
pred.df

## (4) Operational Validation

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )
)
forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)
# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.clean$SalePrice-forward.test)/test.clean$SalePrice;
backward.testPCT <- abs(test.clean$SalePrice-backward.test)/test.clean$SalePrice;
stepwise.testPCT <- abs(test.clean$SalePrice-stepwise.test)/test.clean$SalePrice;
junk.testPCT <- abs(test.clean$SalePrice-junk.test)/test.clean$SalePrice;
# Assign Prediction Grades;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )
)
forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)

# Training Data
# Abs Pct Error
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
backward.PredictionGrade <- ifelse(backward.pct<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(backward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(backward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )
)
backward.trainTable <- table(backward.PredictionGrade)
backward.trainTable/sum(backward.trainTable)
# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.clean$SalePrice-forward.test)/test.clean$SalePrice;
backward.testPCT <- abs(test.clean$SalePrice-backward.test)/test.clean$SalePrice;
stepwise.testPCT <- abs(test.clean$SalePrice-stepwise.test)/test.clean$SalePrice;
junk.testPCT <- abs(test.clean$SalePrice-junk.test)/test.clean$SalePrice;
# Assign Prediction Grades;
backward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )
)
backward.testTable <-table(backward.testPredictionGrade)
backward.testTable/sum(backward.testTable)

# Training Data
# Abs Pct Error
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
stepwise.PredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0,0.10]',
                                   ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )
)
stepwise.trainTable <- table(stepwise.PredictionGrade)
stepwise.trainTable/sum(stepwise.trainTable)
# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.clean$SalePrice-forward.test)/test.clean$SalePrice;
backward.testPCT <- abs(test.clean$SalePrice-backward.test)/test.clean$SalePrice;
stepwise.testPCT <- abs(test.clean$SalePrice-stepwise.test)/test.clean$SalePrice;
junk.testPCT <- abs(test.clean$SalePrice-junk.test)/test.clean$SalePrice;
# Assign Prediction Grades;
stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]',
                                       ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                              ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                     'Grade 4: (0.25+]')
                                       )
)
stepwise.testTable <-table(stepwise.testPredictionGrade)
stepwise.testTable/sum(stepwise.testTable)


# Training Data
# Abs Pct Error
junk.pct <- abs(junk.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
junk.PredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0,0.10]',
                                   ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )
)
junk.trainTable <- table(junk.PredictionGrade)
junk.trainTable/sum(junk.trainTable)
# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.clean$SalePrice-forward.test)/test.clean$SalePrice;
backward.testPCT <- abs(test.clean$SalePrice-backward.test)/test.clean$SalePrice;
stepwise.testPCT <- abs(test.clean$SalePrice-stepwise.test)/test.clean$SalePrice;
junk.testPCT <- abs(test.clean$SalePrice-junk.test)/test.clean$SalePrice;
# Assign Prediction Grades;
junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0.0.10]',
                                       ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                              ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                     'Grade 4: (0.25+]')
                                       )
)
junk.testTable <-table(junk.testPredictionGrade)
junk.testTable/sum(junk.testTable)
