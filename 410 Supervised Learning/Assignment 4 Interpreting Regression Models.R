ames.df <- readRDS('ames_sample.Rdata')

## Fit the MLR model with the response variable SalePrice and the predictor variables TotalSqftCalc,
## TotalBathCalc, LotFrontage, and LotArea. You should get this fitted model.

model.1 <- lm(SalePrice ~ TotalSqftCalc + TotalBathCalc + LotFrontage ++
                + LotArea, data=ames.df)

summary(model.1)

model.1$coef

## (1)	(5 points) Using Model #1 compute the estimated home price for the typical home with 
## (TotalSqftCalc, TotalBathCalc, LotFrontage,    LotArea) = (2100, 2.5, 75, 11000).

x.1 <- matrix(c(1,2100,2.5,75,11000),5,1)
matrix(model.1$coef,1,5)%*%matrix(x.1,5,1)


## (2)	(5 points) Add 400 sqft to our typical home.  Using Model #1 compute the estimated home price 
## for a home with (TotalSqftCalc, TotalBathCalc, LotFrontage,    LotArea) = (2500, 2.5, 75, 11000).

x.2 <- matrix(c(1,2500,2.5,75,11000),5,1)
matrix(model.1$coef,1,5)%*%matrix(x.2,5,1)


## (3)	(5 points) Add 1.5 bathrooms to our typical home.  Using Model #1 compute the estimated 
## home price for a home with (TotalSqftCalc, TotalBathCalc, LotFrontage, LotArea) = 
## (2100, 4.0, 75, 11000).

x.3 <- matrix(c(1,2100,4.0,75,11000),5,1)
matrix(model.1$coef,1,5)%*%matrix(x.3,5,1)


##(4)	(5 points) Add 4000 sqft of lot size to our typical home.  Using Model #1 compute the estimated 
## home price for a home with (TotalSqftCalc, TotalBathCalc, LotFrontage, LotArea) = 
## (2100, 2.5, 75, 15000).

x.4 <- matrix(c(1,6100,2.5,75,15000),5,1)
matrix(model.1$coef,1,5)%*%matrix(x.4,5,1)

## Fit the MLR model with the response variable log(SalePrice) and the predictor variables TotalSqftCalc, 
## TotalBathCalc, LotFrontage, and LotArea.  

model.2 <- lm(log(SalePrice) ~ TotalSqftCalc + TotalBathCalc + LotFrontage ++
                + LotArea, data=ames.df)

summary(model.2)

model.2$coef

## (5)	(5 points) Jensen's Inequality - f( E[X] ) <= E[ f(X) ] for a convex function f
## Compute the exp(mean(log(ames.df$SalePrice))) and note that it is less than 
## mean(exp(log(ames.df$SalePrice))) = mean(ames.df$SalePrice).

exp(mean(log(ames.df$SalePrice)))

mean(exp(log(ames.df$SalePrice)))

mean(ames.df$SalePrice)

exp(mean(log(ames.df$SalePrice))) <= mean(exp(log(ames.df$SalePrice)))
mean(exp(log(ames.df$SalePrice))) = mean(ames.df$SalePrice)

## (6)	(5 points) Using Model #2 compute the estimated home price for the typical home with 
## (TotalSqftCalc, TotalBathCalc, LotFrontage,    LotArea) = (2100, 2.5, 75, 11000).  
## Name this value v.0.

x.6 <- matrix(c(1,2100,2.5,75,11000),5,1)
v.0 <- matrix(model.2$coef,1,5)%*%matrix(x.6,5,1)
exp(v.0)

## (7)	(5 points) Add 400 sqft to our typical home.  Using Model #2 compute the estimated home price 
## for a home with (TotalSqftCalc, TotalBathCalc, LotFrontage,    LotArea) = (2500, 2.5, 75, 11000).  
## Name this value v.1.

x.7 <- matrix(c(1,2500,2.5,75,11000),5,1)
v.1 <- matrix(model.2$coef,1,5)%*%matrix(x.7,5,1)
exp(v.1)


## (8)	(5 points) Compute the percent increase in the home value from adding 400 sqft to our typical home.  
## Verify your answer by computing exp(v.1) / exp(v.0).  Note that the percent increase is the decimal
## portion, i.e. a factor of 1.23 means a 23% increase.

x.8 <- (exp(v.1)/exp(v.0))-1
round(x.8, 3)


## (9)	(5 points) Add 1.5 bathrooms to our typical home.  Using Model #2 compute the estimated home price
## for a home with (TotalSqftCalc, TotalBathCalc, LotFrontage,    LotArea) = (2100, 4.0, 75, 11000). 
## Name this value v.2.

x.9 <- matrix(c(1,2100,4.0,75,11000),5,1)
v.2 <- matrix(model.2$coef,1,5)%*%matrix(x.9,5,1)
exp(v.2)


## (10) (5 points) Compute the percent increase in the home value from adding 1.5 bathrooms to our typical
## home.  Verify your answer by computing exp(v.2) / exp(v.0).  Note that the percent increase is the 
## decimal portion, i.e. a factor of 1.23 means a 23% increase.

x.10 <- (exp(v.2)/exp(v.0))-1
round(x.10, 3)


## (11) (5 points) Add 4000 sqft of lot size to our typical home.  Using Model #2 compute the estimated 
## home price for a home with (TotalSqftCalc, TotalBathCalc, LotFrontage,    LotArea) = 
## (2100, 2.5, 75, 15000).  Name this value v.3.

x.11 <- matrix(c(1,2100,2.5,75,15000),5,1)
v.3 <- matrix(model.2$coef,1,5)%*%matrix(x.11,5,1)
exp(v.3)


## (12) (5 points) Compute the percent increase in the home value from adding 4000 sqft of lot size to our
## typical home.  Verify your answer by computing exp(v.3) / exp(v.0).  Note that the percent increase
## is the decimal portion, i.e. a factor of 1.23 means a 23% increase.

x.12 <- (exp(v.3)/exp(v.0))-1
round(x.12, 3)

library(moments)

## (13) (10 points) Produce a histogram with n=40 bins and compute the skewness for SalePrice.  
## Color the histogram grey.  Paste the histogram into the template.

hist(ames.df$SalePrice,xlab='Sale Price', main='', n=40, col='grey')
x.13 <- skewness(ames.df$SalePrice)
round(x.13, 3)

## (14) (10 points) Produce a histogram with n=40 bins and compute the skewness for log(SalePrice).  
## Color the histogram blue.  Paste the histogram into the template.

hist(log(ames.df$SalePrice),xlab=' log Sale Price', main='', n=40, col='blue')
x.14 <- skewness(log(ames.df$SalePrice))
round(x.14, 3)

## (15) (10 points) Produce a histogram with n=40 bins and compute the skewness for sqrt(SalePrice). 
## Color the histogram red.  Paste the histogram into the template.

hist(sqrt(ames.df$SalePrice),xlab=' Sqrt Sale Price', main='', n=40, col='red')
x.15 <- skewness(sqrt(ames.df$SalePrice))
round(x.15, 3)

## (16) (10 points) Produce a histogram with n=40 bins and compute the skewness for BoxCox(SalePrice). 
## Color the histogram green.  Paste the histogram into the template.
library(forecast)

lambda.hat <- BoxCox.lambda(ames.df$SalePrice)
y.lambda <- BoxCox(ames.df$SalePrice, lambda=lambda.hat)

hist(y.lambda,xlab='Box-Cox Transformed Sale Price', main='', n=40, col='green')
x.16 <- skewness(y.lambda)
round(x.16, 3)
