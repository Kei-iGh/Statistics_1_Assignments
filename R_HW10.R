#Here we want to construct a CI for the Variance of some generated data from Normal Distribution. To that end,
#1. Fix the Random Number Generator's Seed to 1234567;
#2. Generate a Sample x of size 74 from N(-2, 3.72^2) (so the true parameters are m = -2 and s^2 = 3.72^2);
#3. For a moment, let's forget that we know the true value of s^2, and assume that our
#Sample comes from N(-2, s^2). Construct a CI of 95% level for s^2 and check if the true value of s^2 is in that Interval.
#4. Next, assume also that we do not know the true mean of the Distribution, so here assume we just know that our Sample is from some Normal Distribution. Construct a
#CI of 95% level for s^2 and check if the true value of s^2 is in that Interval.
#5. For the above case, construct a 95% CI for the Standard Deviation s;
#6. Solve Part 'd.' using 'varTest' function for 'EnvStat' package.



set.seed(1234567)
d1 <- rnorm(74, -2, 3.72)
#Here our sigma^2 = 3.72^2 = 13.8384
mean_ <- -2
a <- 0.05
n <- 74
lq <- qchisq(a/2, df=n)
uq <- qchisq(1-a/2, df=n)
sum_ <- sum((d1 - mean_)^2)
c(sum_/uq, sum_/lq)
#13.8384 belongs to the interval above

lq2 <- qchisq(a/2, df=n-1)
uq2 <- qchisq(1-a/2, df=n-1)
sum_2 <- sum((d1 - mean(d1))^2)
c(sum_2/uq2, sum_2/lq2)
#13.8384 belongs to the interval above

lq3 <- qchisq(a/2, df=n-1)
uq3 <- qchisq(1-a/2, df=n-1)
sum_3 <- sum((d1 - mean(d1))^2)
c(sqrt(sum_3/uq3), sqrt(sum_3/lq3))


#install.packages('EnvStats')
library(EnvStats)
varTest(d1, alternative = "two.sided", conf.level = 0.95, sigma.squared = 13.8384)



#Using the following link, please download the NBA Players data for 1996 - 2019.
#1. Plot the histogram of the heights of players;
#2. Estimate the mean of the player's height;
#3. Construct an Asymptotic CI of level 0.95 for the mean height.

d2 <-read.csv(file = 'all_seasons.csv')
#The data was downloaded directly in my current directory

hist(d2$player_height)
n2 <- length(d2$player_height)
m1_ <- mean(d2$player_height)
qt_ <- qt(1-a/2, n2-1)
d <- sqrt((sum((d2$player_height - m1_)^2))/(n2-1))/sqrt(n2)

c(m1_ - qt_*d, m1_ + qt_*d)




#On a new highway, the speed limit is set to 70 km/h. The police department is claiming that
#the average speed is higher than 70 km/h on that highway, and wants to put an aragachaph.
#In support of their claim, the department is providing data from randomly chosen 10 cars:
#the recorded speeds are (in km/h): 66, 79, 80, 74, 81, 79, 65, 78, 77, 69.
#Is this data providing a sufficient evidence against the Hypothesis that the average speed
#is 70 km/h? Use 1% significance level. You can assume that the car speeds are normally distributed.
#1. Do the Testing by hand, i.e., by calculating Test Statistics value and comparing with Critical Values;
#2. (R) Do the Testing using CIs;
#3. (R) Do the Testing using p-Values.


d3 <- c(66, 79, 80, 74, 81, 79, 65, 78, 77, 69)
a2 <- 0.01
n3<- length(d3)
m2_ <- mean(d3)
qt2_ <- qt(1-a2/2, n3-1)
d2 <- sqrt((sum((d3 - m2_)^2))/(n3-1))/sqrt(n3)

c(m2_ - qt2_*d2, m2_ + qt2_*d2)

#same CI with t.test
t.test(d3, conf.level = 0.99)

#from t.test p-value = 0.0000000000215
#as p-values is much less than our alpha = 0.01, we reject Null hypothesis
# use alternatie = greater here (-2)




#Write the z.test function in the analogue of t.test. Test your function on some Data.

z.test <- function(data, conflvl, mu0, sigma_){
  n <- length(data)
  z1 <- (mean(data) - mu0)/(sigma_/sqrt(n))
  alpha <- 1- conflvl
  z2 <- qnorm(1-a/2)
  if (abs(z1) > z2){
    print("Reject Null Hypothesis")
  } else {
    print("Do Not Reject Null Hypothesis, Retain it")
  }
}

z.test(d1, 0.95, -2, 3.72)

# this is for only two sided case (-5)










