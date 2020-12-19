## Model 1

## (1)	How many observations are in the sample data?

df = 67
k  = 4
n = df+k+1
n

## (2)	Write out the null and alternate hypotheses for the t-test for Beta1.

## (3)	Compute the t- statistic for Beta1.

Beta1 = 2.18604
SE_B1 = 0.41043
T_B1 = Beta1/SE_B1
round(T_B1, 4)

## (4)	Compute the R-Squared value for Model 1.

SSR = 2126.00904
SST = 2756.36857
R_Squared = SSR/SST
round(R_Squared, 4)

## (5)	Compute the Adjusted R-Squared value for Model 1.

SSE = 630.35953
SST = 2756.36857
n = 72
k = 4
Adj_R_Squared = 1-(((SSE)/(n-k-1))/((SST)/(n-1)))
round(Adj_R_Squared, 4)

## (6)	Write out the null and alternate hypotheses for the Overall F-test.

## (7)	Compute the F-statistic for the Overall F-test.

SSR = 2126.00904
k = 4
SSE = 630.35953
n = 72
p = k+1
F = (SSR/k)/(SSE/(n-p))
round(F, 4)

## Model 2

## (8)	Now let's consider Model 1 and Model 2 as a pair of models.  Does Model 1 nest Model 2 or does Model 2 nest Model 1?  Explain.

## (9)	Write out the null and alternate hypotheses for a nested F-test using Model 1 and Model 2.

## (10) Compute the F-statistic for a nested F-test using Model 1 and Model 2. 

SSE_RM = 630.35953
SSE_FM = 572.60911
dim_FM = 7
dim_RM = 5
n = 72
F_nested = ((SSE_RM-SSE_FM)/(dim_FM-dim_RM))/((SSE_FM)/(n-dim_FM))
round(F_nested, 4)

## Additional Questions

## (11) Compute the AIC values for both Model 1 and Model 2.

n = 72
SSE_m1 = 630.35953
p_m1 = 5
AIC_m1 = n*log(SSE_m1/n)+2*p_m1
round(AIC_m1, 3)

n = 72
SSE_m2 = 572.60911
p_m2 = 7
AIC_m2 = n*log(SSE_m2/n)+2*p_m2
round(AIC_m2, 3)

## (12) Compute the BIC values for both Model 1 and Model 2.

n = 72
SSE_m1 = 630.35953
p_m1 = 5
BIC_m1 = -2*log(SSE_m1/n)+p_m1*log(n)
round(BIC_m1, 3)

n = 72
SSE_m2 = 572.60911
p_m2 = 7
BIC_m2 = -2*log(SSE_m2/n)+p_m2*log(n)
round(BIC_m2, 3)



## (13) Verify the t-statistics for the remaining coefficients in Model 1.

#Model 1
Beta2 = 8.27430
SE_B2 = 2.33906
T_B2 = Beta2/SE_B2
round(T_B2, 4)

Beta3 = 0.49182
SE_B3 = 0.26473
T_B3 = Beta3/SE_B3
round(T_B3, 4)

Beta4 = -0.49356
SE_B4 = 2.29431
T_B4 = Beta4/SE_B4
round(T_B4, 4)

##Model 2
Beta1 = 1.97132
SE_B1 = 0.43653
T_B1 = Beta1/SE_B1
round(T_B1, 4)

Beta2 = 9.13895
SE_B2 = 2.30071
T_B2 = Beta2/SE_B2
round(T_B2, 4)

Beta3 = 0.56485
SE_B3 = 0.26266
T_B3 = Beta3/SE_B3
round(T_B3, 4)

Beta4 = 0.33371
SE_B4 = 2.42131
T_B4 = Beta4/SE_B4
round(T_B4, 4)

Beta5 = 1.90698
SE_B5 = 0.76459
T_B5 = Beta5/SE_B5
round(T_B5, 4)

Beta6 = -1.04330
SE_B6 = 0.64759
T_B6 = Beta6/SE_B6
round(T_B6, 4)

## (14) Verify the Mean Square values for Model 1 and Model 2.

ms_m1 = 531.50226
f_m1 = 56.49
mse_m1 = ms_m1/f_m1
round(mse_m1, 3)

ms_m2 = 363.95991
f_m2 = 41.32
mse_m2 = ms_m2/f_m2
round(mse_m2, 3)

## (15) Verify the Root MSE values for Model 1 and Model 2.

ms_m1 = 531.50226
f_m1 = 56.49
mse_m1 = ms_m1/f_m1
rmse_m1 = sqrt(mse_m1)
round(rmse_m1, 3)

ms_m2 = 363.95991
f_m2 = 41.32
mse_m2 = ms_m2/f_m2
rmse_m2 = sqrt(mse_m2)
round(rmse_m2, 3)
