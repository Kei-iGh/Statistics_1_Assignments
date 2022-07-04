#We want to check the results of the Problem 1b, using R. To see the Consistency,
#. Fix some value of lambda; this will be the true value of our parameter (for simulations, we assume we know it);
#. Take large n;
#. Generate a random sample of size n from the Exp(lambda) Distribution;
#. For each k = 1, 2, 3, ..., n, calculate estimator lambda_k, and plot these points on the graph against k,
#joining the points with lines (use the type = "l" as a parameter in plot command);by this, we want to "see" to what estimator lambda_k is approaching;
#. Now, do the 3-rd step several times, and plot on the previous plot, each time using a
#different color; by this, we want to see that for different observations, the limit will be the same;
#. Finally, add a horizontal line y = l, passing through the real value of l. If your code
#is running correctly, and n is large, the curves for the values of estimator lambda_k need to approach this line, showing the Consistency of ^lk.

lambda_ <- 0.280322

n <- 2022

x <- rexp(n, lambda_)
x2 <- rexp(n, lambda_)
x3 <- rexp(n, lambda_)
x4 <- rexp(n, lambda_)


lambda.hat <- mean(x)
lambda.hat

lambda.hats <- sapply(x, mean)

k <- 1:n

plot(lambda.hats, k, type = "l", xlim = c(0, 25), ylim = c(0, 2000))

par(new=T)
lambda.hats2 <- sapply(x2, mean)
plot(lambda.hats2,k,  type = "l", col = "lightpink", xlim = c(0, 25), ylim = c(0, 2000))


plot( lambda.hats, k,type = "l", xlim = c(0, 25), ylim = c(0, 2000))
par(new=T)
lambda.hats3 <- sapply(x3, mean)
plot(lambda.hats3, k, type = "l", col = "lightblue", xlim = c(0, 25), ylim = c(0, 2000))


plot(lambda.hats, k, type = "l", xlim = c(0, 25), ylim = c(0, 2000))
par(new=T)
lambda.hats4 <- sapply(x4, mean)
plot(lambda.hats4,k, type = "l", col = "lightgreen", xlim = c(0, 25), ylim = c(0, 2000))
abline(v = lambda_, col="red", lwd = 2, lty = 2)




plot(lambda.hats, k, type = "l", xlim = c(0, 10))
par(new=T)
lambda.hats4 <- sapply(x4, mean)
plot(lambda.hats4,k, type = "l", col = "orange", xlim = c(0, 10))
abline(v = lambda_, col="red", lwd = 2, lty = 2)


