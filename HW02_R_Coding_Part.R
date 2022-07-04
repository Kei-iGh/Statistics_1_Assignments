x <- c(1, 2, 3, 4, 5, 6)
y <- c(1,2, 3, 4, 5, 99)

#Problem 6
#. Write a function mad1 which will calculate the Mean Absolute Deviation from the
#Mean. Run it on each of the cars Dataset Variables;
#. Write a function mad2 which will calculate the Mean Absolute Deviation from the
#Median. Test it on the same Dataset as above;
#. (Supplementary) Join the previous functions into one, so that the user will be able to
#choose the Center Measure (like we can choose in the R built-in mad function).

mad1 <- function(x) {
  x.bar <- mean(x)
  abs.dev <- abs(x - x.bar)
  mad_from_mean <- mean(abs.dev)
  return(mad_from_mean)
}

# examples
mad1(x)
mad1(y)
#

mad1(cars$speed)
mad1(cars$dist)

mad2 <- function(x) {
  x.med <- median(x)
  abs.dev <- abs(x - x.med)
  mad_from_median <- mean(abs.dev)
  return(mad_from_median)
}

# examples
mad2(x)
mad2(y)
#

mad2(cars$speed)
mad2(cars$dist)

mad3 <- function(x, type){
  if (type == "Mean") {
    x.bar <- mean(x)
    abs.dev <- abs(x - x.bar)
    mad_from_mean <- mean(abs.dev)
    return(mad_from_mean)
  }
  else if (type == "Median") {
    x.med <- median(x)
    abs.dev <- abs(x - x.med)
    mad_from_median <- mean(abs.dev)
    return(mad_from_median)
  }
  else {
    return("Unknown input")
  }
}

mad1(cars$speed)
mad3(cars$speed, type = "Mean")

mad2(cars$dist)
mad3(cars$dist, type = "Median")



#Problem 7
#Construct the Boxplot of the Paperwork part Problem 3a. Dataset using R, in a horizontal
#position, with the green color.

#Construct, on the same graph, the Boxplots for the Petal.Width variable for each type of
#the Species in the iris Dataset. Give some information you can read from this comparative plot.

a <- c(25, -10, 3, 1, 2, 8, 4, 0, -1, 7, 7, 2, -1, 2, -6, 5, 0)

boxplot(a, horizontal = T, col = "green")

boxplot(Petal.Width~Species, data=iris, horizontal = T)
#For all three variable, we can see that the mean is equal to (or very close to)
#first quartile, which means that the data is right-skewed (most of the data
#points are located to the left of the mean). 
#However, the boxplot for the variable virginica has longer whiskers on the left
#side, so we cannot be 100% sure about the skewness of that data.
#We can also see that only the boxplot of setosa has outliers(2 of them, located
#on the right side). For the boxplot of versicolor,we can see that approximately
#75% of the data is locaetd on the interval from 1 to 1.5, while remaining 25% 
#is from 1.5 to ~ 1.85. 



#Problem 8
#Write an R function which will be calculate the Sample Quantiles of a vector as we have defined in our lectures.

quant <- function(x, percent){
  q <- x[floor(length(x) * percent*0.01)]
  return(q)
}
v <- c(-3, -2, 2, 3, 4, 5, 5, 7, 8)
quant(v, 20)
quant(v, 50)
quant(v, 60)
