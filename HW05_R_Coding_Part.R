x <- rnorm(34, mean = 78.5, sd = 19.4)
y<- rnorm(25, mean = 75.6, sd = 16.9)

#. (R) Use R-s t.test to perform the Test. Give your decision based on the output.

t.test(x, y, alternative="less")

# t belongs to the 95% CI, therefore, we retain H0(which was that mean1 - mean2 = 0)
# also, p-value > 0.05 therefore, again, we retain H0



#Assume we have a Normal Data, and want to test the Hypothesis
#H0 : m = 2 vs H1 : m < 2.
#We will use the following Data to perform the Test:
#1. Find the p-Value of the Test;
#2. For which values of the Significance the Null Hypothesis will be Rejected?

data_ <- c(2.39,  0.57, 0.37 ,1.55 ,4.06, 1.50, 9.00 ,8.89, -1.37, 1.05, 0.70,6.92,
           5.08, 2.08, 0.63, 0.76, -1.00, -0.29 ,-1.03 ,-3.84, 1.82 ,4.51, 0.47, 4.66,
           5.32, 2.53, 5.26 ,2.14 ,1.83, -1.63, 6.65, 3.36, -1.02, -0.06, 1.98, 4.29,
           2.70, 2.80, -9.35, -3.12, 4.90 ,4.11 ,5.90 ,1.89 ,-3.37, 8.68, -0.27, 2.05,-1.69, 4.26)
mu0 <- 2
t <- (mean(data_) - mu0)/(sd(data_)/sqrt(50))
pt(t, df = 50-1)
#p-value = 0.493849
t.test(data_, mu=mu0, alternative="less")
#also can get p-value from t-test (p-value = 0.4938)

#H0 will be rejected for alpha belongs to (0.5, 1), in other words, for CI equal to or less than 50%



#Assume we have the following Data:
#1. Test, at 5% level, if this Dataset is from some Normal Distribution;
#2. Test, at 8% level, if this Dataset is from N(1.1, 0.75^2).
  
data2_ <- c(2.81, 3.09, 0.23, 0.57, -0.27, 0.80, 1.64, 1.84, 0.74, -0.08, 2.91, 1.81,
           2.12 ,0.85, 0.57, 3.42, -0.55, 2.57, 2.46, 0.31, 0.69 ,0.66 ,3.96, 2.14,
           1.09, 1.37, -1.39 ,0.85 ,0.27, 0.38 ,3.46, 2.60, 0.26, 0.91, 3.21, 1.81,
           0.86, 0.43 ,2.79, 2.99, 1.33, 0.59, 0.60 ,0.24 ,-0.46, 1.54, 0.29, -0.11,
           1.13 ,2.28, 0.95, 1.75, 1.38, 2.25 ,-0.12, 2.08, 4.08 ,2.67, 1.57, 1.55,
           1.37, 1.26, 0.42 ,1.13, 2.75, 2.04, 0.69 ,1.63 ,0.72)


shapiro.test(data2_)
#p-value > 0.05, therefore, retain H0 (H0 is: The Dataset data2_ is coming from some normal distribution)

ks.test(data2_, y = "pnorm", mean = 1.1 , sd = 0.75)
#p-value < 0.08, therefore, reject H0 (H0 is: The Dataset data2_ is coming from N(1.1, 0.75^2)) 



