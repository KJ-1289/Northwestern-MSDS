##Assignment 9 Modeling Count Data
options(scipen = 999)
library(MASS)
library(pscl)
setwd("C:/Users/KJohn/OneDrive/Desktop/Northwestern/MSDS 410 Data Modeling for Supervised Learning/Assignment 9 Modeling Count Data")
med.care <- read.table("medical_care.txt")
colnames(med.care) <- c("ofp", "ofnp", "opp", "opnp", "emr", "hosp", "exclhlth",
                        "poorhlth", "numchron", "adldiff", "noreast", "midwest", "west",
                        "age", "black", "male", "married", "school", "faminc", "employed",
                        "privins", "medicaid")
str(med.care)

#Train Test Split
my.data <- subset(med.care, select = c("ofp", "exclhlth", "poorhlth", "numchron", 
                                        "adldiff", "noreast", "midwest", "west", "age", 
                                        "black", "male", "married", "school", "faminc", 
                                        "employed", "privins", "medicaid"))
str(my.data)

set.seed(789)
my.data$u <- runif(n=dim(my.data)[1],min=0,max=1)
train.df <- subset(my.data, u<0.50)
test.df <- subset(my.data, u>=0.50)
Table <- c('Train', 'Test', 'Total')
dim.train <- dim(train.df)[1]
dim.test <- dim(test.df)[1]
dim.total <- dim.train + dim.test
dims <- c(dim.train, dim.test, dim.total)
pct.train <- dim.train/dim.total
pct.test <- dim.test/dim.total
pct.total <- dim.total/4406
pcts <- c(pct.train, pct.test, pct.total)
Table.df <- data.frame(Table, dims, pcts)
Table.df

train.df <- subset(train.df, select = c("ofp", "exclhlth", "poorhlth", "numchron", 
                                       "adldiff", "noreast", "midwest", "west", "age", 
                                       "black", "male", "married", "school", "faminc", 
                                       "employed", "privins", "medicaid"))

test.df <- subset(test.df, select = c("ofp", "exclhlth", "poorhlth", "numchron", 
                                        "adldiff", "noreast", "midwest", "west", "age", 
                                        "black", "male", "married", "school", "faminc", 
                                        "employed", "privins", "medicaid"))
#######################
##Poission Regression##
#######################

full.1 <- glm(ofp ~ ., data=train.df, family=poisson);
summary(full.1)

Poisson.Reg <- stepAIC(full.1,scope=list(upper=full.1,lower=~1),direction=c('backward'));
summary(Poisson.Reg)

#####################################
##Poission Regression w/ Dispersion##
#####################################

full.2 <- glm(ofp ~ ., data=train.df, family=poisson(link=log));
summary(full.2)

Poisson.Dis <- stepAIC(full.2,scope=list(upper=full.2,lower=~1),direction=c('backward'));
summary(Poisson.Dis)

################################
##Negative Binomial Regression##
################################

full.3 <- glm.nb(ofp ~ ., data=train.df)
summary(full.3)

Neg.Binomial <- stepAIC(full.3,scope=list(upper=full.3,lower=~1),direction=c('backward'));
summary(Neg.Binomial)

#####################
##Hurdle Regression##
#####################

full.4 <- hurdle(ofp ~ ., data=train.df)
summary(full.4)

Hurdle.Reg <- stepAIC(full.4,scope=list(upper=full.4,lower=~1),direction=c('backward'));
summary(Hurdle.Reg)

############################
##Zero Inflated Regression##
############################

full.5 <- zeroinfl(ofp ~ ., data=train.df)
summary(full.5)

Zero.Inf <- stepAIC(full.5,scope=list(upper=full.5,lower=~1),direction=c('backward'));
summary(Zero.Inf)

########################
##In Sample Comparison##
########################
library(Metrics)
aic.p <- AIC(Poisson.Reg)
aic.pd <- AIC(Poisson.Dis)
aic.nb <- AIC(Neg.Binomial)
aic.h <- AIC(Hurdle.Reg)
aic.zi <- AIC(Zero.Inf)
bic.p <- BIC(Poisson.Reg)
bic.pd <- BIC(Poisson.Dis)
bic.nb <- BIC(Neg.Binomial)
bic.h <- BIC(Hurdle.Reg)
bic.zi <- BIC(Zero.Inf)
mse.p <- mse(train.df$ofp, Poisson.Reg$fitted.values)
mse.pd <- mse(train.df$ofp, Poisson.Dis$fitted.values)
mse.nb <- mse(train.df$ofp, Neg.Binomial$fitted.values)
mse.h <- mse(train.df$ofp, Hurdle.Reg$fitted.values)
mse.zi <- mse(train.df$ofp, Zero.Inf$fitted.values)
mae.p <- mae(train.df$ofp, Poisson.Reg$fitted.values)
mae.pd <- mae(train.df$ofp, Poisson.Dis$fitted.values)
mae.nb <- mae(train.df$ofp, Neg.Binomial$fitted.values)
mae.h <- mae(train.df$ofp, Hurdle.Reg$fitted.values)
mae.zi <- mae(train.df$ofp, Zero.Inf$fitted.values)

grade.p <- abs(Poisson.Reg$fitted.values-train.df$ofp)
gradetwo.p <- which(grade.p <= 2)
lengrade.p <- length(gradetwo.p)
obs <- length(train.df)
plusminus.p <- lengrade.p/obs

grade.pd <- abs(Poisson.Dis$fitted.values-train.df$ofp)
gradetwo.pd <- which(grade.pd <= 2)
lengrade.pd <- length(gradetwo.pd)
obs <- length(train.df)
plusminus.pd <- lengrade.pd/obs

grade.nb <- abs(Neg.Binomial$fitted.values-train.df$ofp)
gradetwo.nb <- which(grade.nb <= 2)
lengrade.nb <- length(gradetwo.nb)
obs <- length(train.df)
plusminus.nb <- lengrade.nb/obs

grade.h <- abs(Hurdle.Reg$fitted.values-train.df$ofp)
gradetwo.h <- which(grade.h <= 2)
lengrade.h <- length(gradetwo.h)
obs <- length(train.df)
plusminus.h <- lengrade.h/obs

grade.zi <- abs(Zero.Inf$fitted.values-train.df$ofp)
gradetwo.zi <- which(grade.zi <= 2)
lengrade.zi <- length(gradetwo.zi)
obs <- length(train.df)
plusminus.zi <- lengrade.zi/obs

Model <- c('Poisson', 'Poisson w/ Dis.', 'Negative Bi', 'Hurdle', 'Zero Inf')
AIC <- c(aic.p, aic.pd, aic.nb, aic.h, aic.zi)
BIC <- c(bic.p, bic.pd, bic.nb, bic.h, bic.zi)
MSE <- c(mse.p, mse.pd, mse.nb, mse.h, mse.zi)
MAE <- c(mae.p, mae.pd, mae.nb, mae.h, mae.zi)
PM2 <- c(plusminus.p, plusminus.pd, plusminus.nb, plusminus.h, plusminus.zi)
scores.df <- data.frame(Model, AIC, BIC, MSE, MAE, PM2)
scores.df

############################
##Out of Sample Comparison##
############################
p.test <- predict(Poisson.Reg,newdata=test.df)
d.test <- predict(Poisson.Dis,newdata=test.df)
nb.test <- predict(Neg.Binomial,newdata=test.df)
h.test <- predict(Hurdle.Reg,newdata=test.df)
zi.test <- predict(Zero.Inf,newdata=test.df)

mse.pt <- mse(test.df$ofp, p.test)
mse.dt <- mse(test.df$ofp, d.test)
mse.nbt <- mse(test.df$ofp, nb.test)
mse.ht <- mse(test.df$ofp, h.test)
mse.zit <- mse(test.df$ofp, zi.test)
mae.pt <- mae(test.df$ofp, p.test)
mae.dt <- mae(test.df$ofp, d.test)
mae.nbt <- mae(test.df$ofp, nb.test)
mae.ht <- mae(test.df$ofp, h.test)
mae.zit <- mae(test.df$ofp, zi.test)

grade.pt <- abs(p.test-test.df$ofp)
gradetwo.pt <- which(grade.pt <= 2)
lengrade.pt <- length(gradetwo.pt)
obs <- length(test.df)
plusminus.pt <- lengrade.pt/obs

grade.pdt <- abs(d.test-test.df$ofp)
gradetwo.pdt <- which(grade.pdt <= 2)
lengrade.pdt <- length(gradetwo.pdt)
obs <- length(test.df)
plusminus.pdt <- lengrade.pdt/obs

grade.nbt <- abs(nb.test-test.df$ofp)
gradetwo.nbt <- which(grade.nbt <= 2)
lengrade.nbt <- length(gradetwo.nbt)
obs <- length(test.df)
plusminus.nbt <- lengrade.nbt/obs

grade.ht <- abs(h.test-test.df$ofp)
gradetwo.ht <- which(grade.ht <= 2)
lengrade.ht <- length(gradetwo.ht)
obs <- length(test.df)
plusminus.ht <- lengrade.ht/obs

grade.zit <- abs(zi.test-test.df$ofp)
gradetwo.zit <- which(grade.zit <= 2)
lengrade.zit <- length(gradetwo.zit)
obs <- length(test.df)
plusminus.zit <- lengrade.zit/obs

Model <- c('Poisson', 'Poisson w/ Dis.', 'Negative Bi', 'Hurdle', 'Zero Inf')
MSE <- c(mse.pt, mse.dt, mse.nbt, mse.ht, mse.zit)
MAE <- c(mae.pt, mae.dt, mae.nbt, mae.ht, mae.zit)
PM2 <- c(plusminus.pt, plusminus.pdt, plusminus.nbt, plusminus.ht, plusminus.zit)
scores.df <- data.frame(Model, MSE, MAE, PM2)
scores.df

##########################
##Patient Classification##
##########################
train.classification <- ifelse(train.df$ofp<=5,'Segment 1',
                              ifelse(train.df$ofp<=10,'Segment 2',
                                     'Segment 3'))
test.classification <- ifelse(test.df$ofp<=5,'Segment 1',
                               ifelse(test.df$ofp<=10,'Segment 2',
                                      'Segment 3'))

p.classification <- ifelse(Poisson.Reg$fitted.values<=5,'Segment 1',
                                   ifelse(Poisson.Reg$fitted.values<=10,'Segment 2',
                                                 'Segment 3'))
pt.classification <- ifelse(p.test<=5,'Segment 1',
                            ifelse(p.test<=10,'Segment 2',
                                   'Segment 3'))

t <- table(p.classification, train.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

t <- table(pt.classification, test.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

########

d.classification <- ifelse(Poisson.Dis$fitted.values<=5,'Segment 1',
                           ifelse(Poisson.Dis$fitted.values<=10,'Segment 2',
                                  'Segment 3'))
dt.classification <- ifelse(d.test<=5,'Segment 1',
                            ifelse(d.test<=10,'Segment 2',
                                   'Segment 3'))

t <- table(d.classification, train.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

t <- table(dt.classification, test.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

########

nb.classification <- ifelse(Neg.Binomial$fitted.values<=5,'Segment 1',
                           ifelse(Neg.Binomial$fitted.values<=10,'Segment 2',
                                  'Segment 3'))
nbt.classification <- ifelse(nb.test<=5,'Segment 1',
                            ifelse(nb.test<=10,'Segment 2',
                                   'Segment 3'))

t <- table(nb.classification, train.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

t <- table(nbt.classification, test.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

########

nb.classification <- ifelse(Neg.Binomial$fitted.values<=5,'Segment 1',
                            ifelse(Neg.Binomial$fitted.values<=10,'Segment 2',
                                   'Segment 3'))
nbt.classification <- ifelse(nb.test<=5,'Segment 1',
                             ifelse(nb.test<=10,'Segment 2',
                                    'Segment 3'))

t <- table(nb.classification, train.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

t <- table(nbt.classification, test.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

########

h.classification <- ifelse(Hurdle.Reg$fitted.values<=5,'Segment 1',
                            ifelse(Hurdle.Reg$fitted.values<=10,'Segment 2',
                                   'Segment 3'))
ht.classification <- ifelse(h.test<=5,'Segment 1',
                             ifelse(h.test<=10,'Segment 2',
                                    'Segment 3'))

t <- table(h.classification, train.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

t <- table(ht.classification, test.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

########

zi.classification <- ifelse(Zero.Inf$fitted.values<=5,'Segment 1',
                           ifelse(Zero.Inf$fitted.values<=10,'Segment 2',
                                  'Segment 3'))
zit.classification <- ifelse(zi.test<=5,'Segment 1',
                           ifelse(zi.test<=10,'Segment 2',
                                  'Segment 3'))

t <- table(zi.classification, train.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

t <- table(zit.classification, test.classification)
r <- apply(t, MARGIN = 1, FUN = sum)
t/r

###########################
##Classification Accuracy##
###########################
train.obs <- length(train.classification)
test.obs <- length(test.classification)

p.logic <- p.classification == train.classification
p.true <- sum(p.logic)
p.true
p.acc <- p.true/train.obs

d.logic <- d.classification == train.classification
d.true <- sum(d.logic)
d.true
d.acc <- d.true/train.obs

nb.logic <- nb.classification == train.classification
nb.true <- sum(nb.logic)
nb.true
nb.acc <- nb.true/train.obs

h.logic <- h.classification == train.classification
h.true <- sum(h.logic)
h.true
h.acc <- h.true/train.obs

zi.logic <- zi.classification == train.classification
zi.true <- sum(zi.logic)
zi.true
zi.acc <- zi.true/train.obs

pt.logic <- pt.classification == test.classification
pt.true <- sum(pt.logic)
pt.true
pt.acc <- pt.true/train.obs

dt.logic <- dt.classification == test.classification
dt.true <- sum(dt.logic)
dt.true
dt.acc <- dt.true/train.obs

nbt.logic <- nbt.classification == test.classification
nbt.true <- sum(nbt.logic)
nbt.true
nbt.acc <- nbt.true/train.obs

ht.logic <- ht.classification == test.classification
ht.true <- sum(ht.logic)
ht.true
ht.acc <- ht.true/train.obs

zit.logic <- zit.classification == test.classification
zit.true <- sum(zit.logic)
zit.true
zit.acc <- zit.true/train.obs

Model <- c('Poisson', 'Poisson w/ Dis.', 'Negative Bi', 'Hurdle', 'Zero Inf')
Acc.Train <- c(p.acc, d.acc, nb.acc, h.acc, zi.acc)
Acc.Test <- c(pt.acc, dt.acc, nbt.acc, ht.acc, zit.acc)
scores.df <- data.frame(Model, Acc.Train, Acc.Test)
scores.df
