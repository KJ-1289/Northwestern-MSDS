## Assignment 6: Interpreting ANCOVA Models
setwd("C:/Users/KJohn/OneDrive/Desktop/Northwestern/MSDS 410 Data Modeling for Supervised Learning/Assignment 6 Interpreting ANCOVA Models")
my.df <- read.csv("NutritionStudy.csv",header=TRUE);
head(my.df)
str(my.df)

library(lattice)

##############################################################################
# Conditional scatter plots - Gender;
##############################################################################

pch.1 <- c(19,19);
col.1 <- c('grey','blue');

custom.key <- list(title='',space='bottom',columns=2,
                   text=list(levels(my.df$Gender)),
                   points=list(pch=pch.1,col=col.1)
)

plot.1 <- xyplot(Cholesterol ~ BetaPlasma, groups=Gender, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key,
)

plot.2 <- xyplot(Cholesterol ~ BetaDiet, groups=Gender, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)

plot.3 <- xyplot(Cholesterol ~ RetinolPlasma, groups=Gender, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)

plot.4 <- xyplot(Cholesterol ~ RetinolDiet, groups=Gender, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)


plot(plot.1, split=c(1,1,2,2))
plot(plot.2, split=c(2,1,2,2), newpage=FALSE)
plot(plot.3, split=c(1,2,2,2), newpage=FALSE)
plot(plot.4, split=c(2,2,2,2), newpage=FALSE)






plot.5 <- xyplot(Cholesterol ~ Alcohol, groups=Gender, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key,
)

plot.6 <- xyplot(Cholesterol ~ Calories, groups=Gender, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)

plot.7 <- xyplot(Cholesterol ~ Fat, groups=Gender, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)

plot.8 <- xyplot(Cholesterol ~ Fiber, groups=Gender, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)


plot(plot.5, split=c(1,1,2,2))
plot(plot.6, split=c(2,1,2,2), newpage=FALSE)
plot(plot.7, split=c(1,2,2,2), newpage=FALSE)
plot(plot.8, split=c(2,2,2,2), newpage=FALSE)

##############################################################################
# Conditional scatter plots - Vitamin Use;
##############################################################################

pch.1 <- c(19,1,1);
col.1 <- c('grey','blue','red');

custom.key <- list(title='',space='bottom',columns=3,
                   text=list(levels(my.df$VitaminUse)),
                   points=list(pch=pch.1,col=col.1),
                   cex=0.5
)

plot.1 <- xyplot(Cholesterol ~ BetaPlasma, groups=VitaminUse, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key,
)

plot.2 <- xyplot(Cholesterol ~ BetaDiet, groups=VitaminUse, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)

plot.3 <- xyplot(Cholesterol ~ RetinolPlasma, groups=VitaminUse, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)

plot.4 <- xyplot(Cholesterol ~ RetinolDiet, groups=VitaminUse, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)


plot(plot.1, split=c(1,1,2,2))
plot(plot.2, split=c(2,1,2,2), newpage=FALSE)
plot(plot.3, split=c(1,2,2,2), newpage=FALSE)
plot(plot.4, split=c(2,2,2,2), newpage=FALSE)

plot.5 <- xyplot(Cholesterol ~ Alcohol, groups=VitaminUse, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key,
)

plot.6 <- xyplot(Cholesterol ~ Calories, groups=VitaminUse, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)

plot.7 <- xyplot(Cholesterol ~ Fat, groups=VitaminUse, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)

plot.8 <- xyplot(Cholesterol ~ Fiber, groups=VitaminUse, data=my.df, 
                 col=col.1,	
                 pch=pch.1, cex=1, type=c('p','g'), 
                 layout=c(1,1), aspect=1.0,
                 key=custom.key
)


plot(plot.5, split=c(1,1,2,2))
plot(plot.6, split=c(2,1,2,2), newpage=FALSE)
plot(plot.7, split=c(1,2,2,2), newpage=FALSE)
plot(plot.8, split=c(2,2,2,2), newpage=FALSE)

##############################################################################
# Defining a custom panel function;
##############################################################################
# p.8 in Advanced graphics with the lattice package
# This is also in the lattice book;


my.panel <- function(x,y){
  panel.xyplot(x,y,pch=1)
  panel.rug(x,y)
  panel.grid(h=-1,v=-1)
  panel.lmline(x,y,col='red',lwd=2,lty=2)
}

##############################################################################
# Conditional scatter plots - Gender;
##############################################################################


plot.1 <- xyplot(Cholesterol ~ Alcohol|Gender, data=my.df, 
                 cex=1, type=c('p','g'),
                 layout=c(2,1), aspect=1.5,
                 panel=my.panel
)

plot.2 <- xyplot(Cholesterol ~ Calories|Gender, data=my.df, 
                 cex=1, type=c('p','g'),
                 layout=c(2,1), aspect=1.5,
                 panel=my.panel
)

plot.3 <- xyplot(Cholesterol ~ Fat|Gender, data=my.df, 
                 cex=1, type=c('p','g'),
                 layout=c(2,1), aspect=1.5,
                 panel=my.panel
)

plot.4 <- xyplot(Cholesterol ~ Fiber|Gender, data=my.df, 
                 cex=1, type=c('p','g'),
                 layout=c(2,1), aspect=1.5,
                 panel=my.panel
)


plot(plot.1, split=c(1,1,2,2))
plot(plot.2, split=c(2,1,2,2), newpage=FALSE)
plot(plot.3, split=c(1,2,2,2), newpage=FALSE)
plot(plot.4, split=c(2,2,2,2), newpage=FALSE)

plot.5 <- xyplot(Cholesterol ~ BetaPlasma|Gender, data=my.df, 
                 cex=1, type=c('p','g'),
                 layout=c(2,1), aspect=1.5,
                 panel=my.panel
)

plot.6 <- xyplot(Cholesterol ~ BetaDiet|Gender, data=my.df, 
                 cex=1, type=c('p','g'),
                 layout=c(2,1), aspect=1.5,
                 panel=my.panel
)

plot.7 <- xyplot(Cholesterol ~ RetinolPlasma|Gender, data=my.df, 
                 cex=1, type=c('p','g'),
                 layout=c(2,1), aspect=1.5,
                 panel=my.panel
)

plot.8 <- xyplot(Cholesterol ~ RetinolDiet|Gender, data=my.df, 
                 cex=1, type=c('p','g'),
                 layout=c(2,1), aspect=1.5,
                 panel=my.panel
)


plot(plot.5, split=c(1,1,2,2))
plot(plot.6, split=c(2,1,2,2), newpage=FALSE)
plot(plot.7, split=c(1,2,2,2), newpage=FALSE)
plot(plot.8, split=c(2,2,2,2), newpage=FALSE)

#Model 1
model.1 <- lm(Cholesterol ~ Gender + Fat, data=my.df)
summary(model.1)

model.1b <- lm(Cholesterol ~ Gender$Female + Fat, data=my.df)
summary(model.1b)

#Model 2
model.2 <- lm(Cholesterol ~ Gender:Fat, data=my.df)