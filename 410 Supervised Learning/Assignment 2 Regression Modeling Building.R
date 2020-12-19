#Load packages
library(ggplot2)
library(knitr)
library(dplyr)
library(tidyverse)
library(tibble)
library(dlookr)
library(gridExtra)
library(corrplot)
library(stargazer)

#Read data
mydata <- read.csv("ames_housing_data.csv", header = TRUE, sep = ",")
ames <- data.frame(mydata)
str(ames)

#Filter the data
ames<- mydata %>% dplyr::filter(Zoning %in% c("FV","RH","RL","RM") & BldgType %in% c("1Fam","2fmCon")) 
ames$Zoning<-factor(ames$Zoning)
ames$BldgType<-factor(ames$BldgType)
addmargins(xtabs(~Zoning+BldgType,ames))

#Add new columns
ames$TotalSqftCalc <- ames$FirstFlrSF + ames$SecondFlrSF
ames$TotalBathCalc <- ames$FullBath + ames$HalfBath
ames$QualityIndex <- ames$OverallQual * ames$OverallCond
ames$logSalePrice <- log(ames$SalePrice)
str(ames)

#Subset continuous variables
small.df <- subset(ames, select = c('SalePrice','logSalePrice','TotalSqftCalc',
                                    'TotalBathCalc', 'TotalBsmtSF', 'GarageArea',
                                    'GrLivArea', 'TotRmsAbvGrd', 'LotArea'))
str(small.df)

#Check for missing values, change missing values to 0
na_check <- diagnose(small.df)
small.df[is.na(small.df)] <- 0
na_check

#Correlation matrix
cor <- cor(small.df)
corrplot(cor, method="number", shade.col=NA, tl.col="black",tl.cex=0.5, diag = TRUE, 
         na.label = "square", na.label.col = "grey",  addCoef.col = "black")

#Subset top two variables
model.df <- subset(small.df, select = c('SalePrice','logSalePrice',
                                        'TotalSqftCalc', 'GarageArea'))

#Scatterplots of predictor variables vs. SalePrice
loess.1 <- loess(SalePrice ~ TotalSqftCalc,data=model.df)
lm.1 <- lm(SalePrice ~ TotalSqftCalc,data=model.df);

plot(model.df$TotalSqftCalc, model.df$SalePrice,xlab='TotalSqftCalc',ylab='SalePrice')
points(loess.1$x,loess.1$fitted,type='p',col='red')
abline(coef=lm.1$coef,col='blue',lwd=2)
title('SLR - TotalSqftCalc')

loess.2 <- loess(SalePrice ~ GarageArea,data=model.df)
lm.2 <- lm(SalePrice ~ GarageArea,data=model.df)

plot(model.df$GarageArea, model.df$SalePrice,xlab='GarageArea',ylab='SalePrice')
points(loess.2$x,loess.2$fitted,type='p',col='red')
abline(coef=lm.2$coef,col='blue',lwd=2)
title('SLR - GarageArea')

#Fit a linear regression model
model.1 <- lm(SalePrice ~ TotalSqftCalc, data = model.df)
summary(model.1)

#Make a scatterplot
plot(model.df$TotalSqftCalc, model.1$residuals)
title("Residual vs. Predictor")

# Make a QQPlot
qqnorm(model.1$residuals)
qqline(model.1$residuals)

#Second linear regression model
model.2 <- lm(SalePrice ~ GarageArea, data = model.df)
summary(model.2)

#Make a scatterplot
plot(model.df$GarageArea, model.2$residuals)
title("Residual vs. Predictor")

# Make a QQPlot
qqnorm(model.2$residuals)
qqline(model.2$residuals)

#Multiple regression dataframe
mlrmodel.df <- subset(small.df, select = c('SalePrice','logSalePrice','TotalSqftCalc', 
                                           'GarageArea', 'TotalBsmtSF', 'TotalBathCalc'))

#Scatterplots of predictor variables vs. SalePrice
loess.3 <- loess(SalePrice ~ TotalBsmtSF,data=mlrmodel.df)
lm.3 <- lm(SalePrice ~ TotalBsmtSF,data=mlrmodel.df);

plot(mlrmodel.df$TotalBsmtSF, mlrmodel.df$SalePrice,xlab='TotalBsmtSF',ylab='SalePrice')
points(loess.3$x,loess.3$fitted,type='p',col='red')
abline(coef=lm.3$coef,col='blue',lwd=2)
title('SLR - TotalBsmtSF')

loess.4 <- loess(SalePrice ~ TotalBathCalc,data=mlrmodel.df)
lm.4 <- lm(SalePrice ~ TotalBathCalc,data=mlrmodel.df);

plot(mlrmodel.df$TotalBathCalc, mlrmodel.df$SalePrice,xlab='TotalBathCalc',ylab='SalePrice')
points(loess.4$x,loess.4$fitted,type='p',col='red')
abline(coef=lm.4$coef,col='blue',lwd=2)
title('SLR - TotalBathCalc')

#Multiple linear regression
model.3 <- lm(SalePrice ~ TotalSqftCalc + GarageArea + TotalBsmtSF + TotalBathCalc, 
              data = mlrmodel.df)
summary(model.3)

# Make a QQPlot
qqnorm(model.3$residuals)
qqline(model.3$residuals)

model.3b <- lm(SalePrice ~ TotalSqftCalc + GarageArea + TotalBsmtSF, 
              data = mlrmodel.df)
summary(model.3b)

# Make a QQPlot
qqnorm(model.3b$residuals)
qqline(model.3b$residuals)

#Transformed multiple linear regression
model.4 <- lm(logSalePrice ~ TotalSqftCalc + GarageArea + TotalBsmtSF + TotalBathCalc, 
              data = mlrmodel.df)
summary(model.4)

# Make a QQPlot
qqnorm(model.4$residuals)
qqline(model.4$residuals)

#MSE and MAE Scores
mse.3 <- mean((model.3$residuals)^2)
mae.3 <- mean((abs(model.3$residuals)))
mse.4 <- mean((mlrmodel.df$SalePrice-exp(model.4$fitted.values))^2)
mae.4 <- mean(abs(mlrmodel.df$SalePrice-exp(model.4$fitted.values)))

#Ratios
mse.rat.3 <- mse.3/mse.4
mae.rat.3 <- mae.3/mae.4
mse.rat.4 <- mse.4/mse.3
mae.rat.4 <- mae.4/mae.3

#Create dataframe
Model <- c('Model 3', 'Model 4')
R_Squared <- c(.7188, .7380)
MSE <- c(mse.3, mse.4)
MAE <- c(mae.3, mae.4)
MSE.Ratio <- c(mse.rat.3, mse.rat.4)
MAE.Ratio <- c(mae.rat.3, mae.rat.4)
acc.df <- data.frame(Model, R_Squared, MSE, MAE, MSE.Ratio, MAE.Ratio)
acc.df
