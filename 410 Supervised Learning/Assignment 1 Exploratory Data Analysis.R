#Load packages
library(ggplot2)
library(knitr)
library(dplyr)
library(tidyverse)
library(tibble)
library(dlookr)
library(gridExtra)
library(corrplot)

#Read data
mydata <- read.csv("ames_housing_data.csv", header = TRUE, sep = ",")
ames <- data.frame(mydata)
str(ames)

#Create bar chart
data_types <- function(frame) {
  res <- lapply(frame, class)
  res_frame <- data.frame(unlist(res))
  xx<-barplot(table(res_frame), main="Data Types", col="steelblue", ylab="Number of Features", ylim=c(0, 1.1*max(table(res_frame))))
  text(x = xx, y = table(res_frame), label = table(res_frame), pos = 3, cex = 1)
}
data_types(ames)

#Summarize variables from type
data.frame((ames %>%  summarise_all(typeof)))

#Examine sale price variables
summary(ames$SalePrice)

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

#Subset predictor variables
small.df <- subset(ames, select = c('SalePrice','logSalePrice','TotalSqftCalc',
                                    'TotalBathCalc','QualityIndex','TotRmsAbvGrd',
                                    'OverallQual','OverallCond'))
str(small.df)

#Check for missing values
na_check <- diagnose(small.df)
na_check

#Distribution of sale price
ggplot(small.df, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#Correlation matrix
cor <- cor(small.df)
corrplot(cor, method="number", shade.col=NA, tl.col="black",tl.cex=0.5, diag = TRUE, 
         na.label = "square", na.label.col = "grey",  addCoef.col = "black")

#Scatterplots of predictor variables vs. SalePrice
loess.1 <- loess(SalePrice ~ TotalSqftCalc,data=small.df);
lm.1 <- lm(SalePrice ~ TotalSqftCalc,data=small.df);

plot(small.df$TotalSqftCalc, small.df$SalePrice,xlab='TotalSqftCalc',ylab='SalePrice')
points(loess.1$x,loess.1$fitted,type='p',col='red')
abline(coef=lm.1$coef,col='blue',lwd=2)
title('SLR - TotalSqftCalc')


loess.2 <- loess(SalePrice ~ OverallQual,data=small.df);
lm.2 <- lm(SalePrice ~ OverallQual,data=small.df);

plot(small.df$SalePrice ~ small.df$OverallQual,xlab='OverallQual',ylab='SalePrice')
points(loess.2$x,loess.2$fitted,type='p',col='red',pch=19)
abline(coef=lm.2$coef,col='blue',lwd=2)
title('SLR - OverallQual')


loess.3 <- loess(SalePrice ~ OverallCond,data=small.df);
lm.3 <- lm(SalePrice ~ OverallCond,data=small.df);

plot(small.df$SalePrice ~ small.df$OverallCond,xlab='OverallCond',ylab='SalePrice')
points(loess.3$x,loess.3$fitted,type='p',col='red',pch=19)
abline(coef=lm.3$coef,col='blue',lwd=2)
title('SLR - OverallCond')


loess.4 <- loess(SalePrice ~ TotalBathCalc,data=small.df);
lm.4 <- lm(SalePrice ~ TotalBathCalc,data=small.df);

plot(small.df$SalePrice ~ small.df$TotalBathCalc,xlab='TotalBathCalc',ylab='SalePrice')
points(loess.4$x,loess.4$fitted,type='p',col='red',pch=19)
abline(coef=lm.4$coef,col='blue',lwd=2)
title('SLR - TotalBathCalc')


loess.5 <- loess(SalePrice ~ TotRmsAbvGrd,data=small.df);
lm.5 <- lm(SalePrice ~ TotRmsAbvGrd,data=small.df);

plot(small.df$SalePrice ~ small.df$TotRmsAbvGrd,xlab='TotRmsAbvGrd',ylab='SalePrice')
points(loess.5$x,loess.5$fitted,type='p',col='red',pch=19)
abline(coef=lm.5$coef,col='blue',lwd=2)
title('SLR - TotRmsAbvGrd')


loess.6 <- loess(SalePrice ~ QualityIndex,data=small.df);
lm.6 <- lm(SalePrice ~ QualityIndex,data=small.df);

plot(small.df$SalePrice ~ small.df$QualityIndex,xlab='QualityIndex',ylab='SalePrice')
points(loess.6$x,loess.6$fitted,type='p',col='red',pch=19)
abline(coef=lm.6$coef,col='blue',lwd=2)
title('SLR - QualityIndex')

#Scatterplots of predictor variables vs. logSalePrice
loess.1 <- loess(logSalePrice ~ TotalSqftCalc,data=small.df);
lm.1 <- lm(logSalePrice ~ TotalSqftCalc,data=small.df);

plot(small.df$TotalSqftCalc, small.df$logSalePrice,xlab='TotalSqftCalc',ylab='logSalePrice')
points(loess.1$x,loess.1$fitted,type='p',col='red')
abline(coef=lm.1$coef,col='blue',lwd=2)
title('SLR - TotalSqftCalc')


loess.2 <- loess(logSalePrice ~ OverallQual,data=small.df);
lm.2 <- lm(logSalePrice ~ OverallQual,data=small.df);

plot(small.df$logSalePrice ~ small.df$OverallQual,xlab='OverallQual',ylab='logSalePrice')
points(loess.2$x,loess.2$fitted,type='p',col='red',pch=19)
abline(coef=lm.2$coef,col='blue',lwd=2)
title('SLR - OverallQual')


loess.3 <- loess(logSalePrice ~ OverallCond,data=small.df);
lm.3 <- lm(logSalePrice ~ OverallCond,data=small.df);

plot(small.df$logSalePrice ~ small.df$OverallCond,xlab='OverallCond',ylab='logSalePrice')
points(loess.3$x,loess.3$fitted,type='p',col='red',pch=19)
abline(coef=lm.3$coef,col='blue',lwd=2)
title('SLR - OverallCond')


loess.4 <- loess(logSalePrice ~ TotalBathCalc,data=small.df);
lm.4 <- lm(logSalePrice ~ TotalBathCalc,data=small.df);

plot(small.df$logSalePrice ~ small.df$TotalBathCalc,xlab='TotalBathCalc',ylab='logSalePrice')
points(loess.4$x,loess.4$fitted,type='p',col='red',pch=19)
abline(coef=lm.4$coef,col='blue',lwd=2)
title('SLR - TotalBathCalc')


loess.5 <- loess(logSalePrice ~ TotRmsAbvGrd,data=small.df);
lm.5 <- lm(logSalePrice ~ TotRmsAbvGrd,data=small.df);

plot(small.df$logSalePrice ~ small.df$TotRmsAbvGrd,xlab='TotRmsAbvGrd',ylab='logSalePrice')
points(loess.5$x,loess.5$fitted,type='p',col='red',pch=19)
abline(coef=lm.5$coef,col='blue',lwd=2)
title('SLR - TotRmsAbvGrd')


loess.6 <- loess(logSalePrice ~ QualityIndex,data=small.df);
lm.6 <- lm(logSalePrice ~ QualityIndex,data=small.df);

plot(small.df$logSalePrice ~ small.df$QualityIndex,xlab='QualityIndex',ylab='logSalePrice')
points(loess.6$x,loess.6$fitted,type='p',col='red',pch=19)
abline(coef=lm.6$coef,col='blue',lwd=2)
title('SLR - QualityIndex')


#Boxplots
totrmsabvgrad_box <- ggplot(small.df, aes(x= TotRmsAbvGrd, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Total Rooms Abv Grd vs Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

totrmsabvgrad_box_log <- ggplot(small.df, aes(x= TotRmsAbvGrd, y=logSalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Total Rooms Abv Grd vs log Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

overallqual_box <- ggplot(small.df, aes(x= OverallQual, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Overall Qual vs Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

overallqual_box_log <- ggplot(small.df, aes(x= OverallQual, y=logSalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Overall Qual vs log Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

overallcond_box <- ggplot(small.df, aes(x= OverallCond, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Overall Cond vs Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

overallcond_box_log <- ggplot(small.df, aes(x= OverallCond, y=logSalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Overall Cond vs log Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

grid.arrange(totrmsabvgrad_box, totrmsabvgrad_box_log, overallqual_box, overallqual_box_log,
             overallcond_box, overallcond_box_log, ncol=2, nrow=3)
