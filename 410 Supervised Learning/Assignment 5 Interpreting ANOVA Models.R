##Assignment 5
##Interpreting ANOVA Models

library(moments)

# Enhanced histogram helper function
enhanced_hist <- function(x, title_alias) {
  par(mfrow=c(1, 2))
  skew <- round(moments::skewness(x), digits = 3)
  kurtosis <- round(moments::kurtosis(x), digits = 3)
  
  #Histogram
  hist(
    x = x,
    main = paste("Histogram of", title_alias),
    xlab = title_alias,
    col = "purple"
  )
  legend("topright",
         legend = paste("kurtosis =", kurtosis, "&", "skew =", skew))
  
  # Boxplot
  boxplot(
    x = x,
    main = paste("Boxplot of", title_alias),
    xlab = title_alias,
    col = "purple",
    outcol = "red"
  )
}
  
## F-test helper function
# Omnibus F-test calculation
omnibus_f <- function(model, alpha = 0.95) {
  # Calculate F-statistic
  anova_obj <- anova(model)
  ssy <- sum(anova_obj$`Sum Sq`)
  sse_index <- length(anova_obj$`Sum Sq`)
  sse <- anova_obj$`Sum Sq`[sse_index]
  k <- sum(anova_obj$Df[-sse_index])
  n <- sum(anova_obj$Df) + 1
  num <- (ssy - sse) / k
  denom <- sse / (n - k - 1)
  f <- round(num / denom, digits = 4)
  
  # Calculate critical F Value
  crit_f <- round(qf(alpha, k, (n - k - 1)), digits = 4)
  
  # Output: Determine if reject the null
  if (f > crit_f) {
    print(paste("F-statistic of", f, "is greater than the critical value of", crit_f))
    print("We can REJECT the null hypothesis")
  } else {
    print(paste("F-statistic of", f, "is less than the critical value of", crit_f))
    print("We FAIL TO REJECT the null hypothesis")
  }
}

data.path <- 'C:\\Users\\Chad R Bhatti\\Dropbox\\Northwestern_MSPA\\MSDS_410_2020\\NutritionStudy_Files\\'
data.file <- paste('NutritionStudy.csv',sep='')

my.df <- read.csv(data.file,header=TRUE)
head(my.df)
str(my.df)

enhanced_hist(my.df$Cholesterol, "Cholesterol")
summary(my.df$Cholesterol)

# Define some indicator variables;
my.df$SmokeYes <- ifelse(my.df$Smoke=='Yes',1,0)
my.df$Male <- ifelse(my.df$Gender=='Male',1,0)
my.df$RegularVitamin <- ifelse(my.df$VitaminUse=='Regular',1,0)


## Question 1: ANOVA regression for gender
model.1 <- lm(Cholesterol ~ Gender, data=my.df)
summary(model.1)

male.cholesterol <- subset(my.df$Cholesterol,my.df$Gender=='Male')
female.cholesterol <- subset(my.df$Cholesterol,my.df$Gender=='Female')

model.1.matrix <- matrix(nrow = 1, ncol = 3, dimnames = list(c("Cholesterol"), c("All", "Male", "Female")))
model.1.matrix[c(1)] = mean(my.df$Cholesterol)
model.1.matrix[c(2)] = mean(male.cholesterol)
model.1.matrix[c(3)] = mean(female.cholesterol)
model.1.matrix

omnibus_f(model.1)

## Question 2: Regression model with indicator Gender=='Male'
model.2 <- lm(Cholesterol ~ Male, data=my.df);
summary(model.2)

## Question 3: ANOVA regression for Smoke
model.3 <- lm(Cholesterol ~ Smoke, data=my.df);
summary(model.3)
omnibus_f(model.3)

## Question 4: Use a 2x2 ANOVA regression model to compute the four means.
## Verify these means using the model and their separate computations.
model.4 <- lm(Cholesterol ~ Gender*Smoke, data=my.df);
summary(model.4)

male.smoker <- subset(my.df$Cholesterol,(my.df$Gender=='Male')&(my.df$Smoke=='Yes'));
male.nonsmoker <- subset(my.df$Cholesterol,(my.df$Gender=='Male')&(my.df$Smoke=='No'));
female.smoker <- subset(my.df$Cholesterol,(my.df$Gender=='Female')&(my.df$Smoke=='Yes'));
female.nonsmoker <- subset(my.df$Cholesterol,(my.df$Gender=='Female')&(my.df$Smoke=='No'));

my.matrix <- matrix(nrow = 2, ncol = 2, dimnames = list(c("Male", "Female"), c("Smoker", "Nonsmoker")))
my.matrix[c(1)] = mean(male.smoker)
my.matrix[c(3)] = mean(male.nonsmoker)
my.matrix[c(2)] = mean(female.smoker)
my.matrix[c(4)] = mean(female.nonsmoker)
my.matrix

