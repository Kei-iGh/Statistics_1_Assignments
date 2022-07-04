#R Coding Part
#Problem 3, Histograms

#a.
#Consider one of the standard Datasets in R, islands.
#. call the help page for this Dataset to see the description
#. print the structure of the Dataset
#. print the head of this Dataset
#. plot the Frequency Histogram for the islands with the area less than 200,000 sq miles
#. plot the Density Histogram for the islands with the area less than 200,000 sq miles

?islands

str(islands)

head(islands)

hist(islands[islands < 200])

hist(islands[islands < 200], freq = FALSE, ylim = c(0,0.02))


#b.
#Here we want to check that the Density Histogram is approximating well the PDF behind the data.

#. Take n = 1000
#. generate a sample of size n from the Weibull distribution with the shape parameter 2
#. plot the Density Histogram of that sample, in cyan color
#. plot the theoretical PDF (use dweibull in R) over the previous graph, in red, and with linewidth 3.
#Note: Adjust the scales of axes for both graphs!

n = 1000

t <- rweibull(n, shape = 2)
t

hist(t, col = "cyan", freq = FALSE, ylim = c(0, 1), xlim = c(0, 3))
par(new = TRUE)
plot(seq(0, 3, 0.01), dweibull(seq(0, 3, 0.01), shape = 2), type = "l", col = "red", lwd = 3, ylim = c(0, 1), xlim = c(0, 3),axes = FALSE, xlab = "", ylab = "")


#c.
#Now let's plot comparative Histograms. We will work with the R-s default ChickWeight Dataset.

#. Explore the Dataset: read the description and print the first 5 rows of that Dataset;
#. Separate in x the Weight variable for all Chicken with the Diet 1;
#. Separate in y the Weight variable for all Chicken with the Diet 2;
#. Calculate the Median of Weights for Chickens fed with the first diet;
#. Calculate the difference between the Means of Weights for Chickens fed with the first and second diets;
#. Plot the Frequency Histograms of x and y one over another. You can use transparent colors to make your graphs look nicer.

?ChickWeight
head(ChickWeight, 5)

x <- ChickWeight$weight[ChickWeight$Diet == 1]
x

y <- ChickWeight$weight[ChickWeight$Diet == 2]
y

median(x)

first_d <- mean(x)
second_d <- mean(y)
abs(c(first_d - second_d))

library(scales)
hist(x, col = alpha("darkred", 0.5), ylim = c(0, 100))
par(new=TRUE)
hist(y, col = alpha("yellowgreen", 0.5), ylim = c(0, 100))


#Problem 4, ScatterPlot

#a.
#Plot the following points:
#(0, 2), (3, -1), (4, 2), (5, 5), (-1, 2)

a <- c(0, 3, 4, 5, -1)
b <- c(2, -1, 2, 5, 2)
plot(a, b)

#b.
#R-s pressure Dataset consists of 2 Variables. Give the ScatterPlot of these Variables.

?pressure
plot(pressure$temperature, pressure$pressure)


#Problem 5, Apple Stock Weekly Returns Histogram
#Go to Yahoo Finance page, navigate to the Apple Stock page (Apple's symbol (ticker) is
#AAPL, make a search for it), then choose Historical Data, 5 years time period, and weekly
#frequency. Download that Data. It will be in .csv format.
#. Using the R read.csv command, extract the Adjusted Close Prices ("Adj Close"column), 
#calculate weekly returns of the Apple stock

aapl <- read.csv(file.choose())
Adj <- aapl$Adj.Close
Adj

aapl_weekly_returns <- c(diff(Adj) / Adj[-length(Adj)])
aapl_weekly_returns

#or could also use an existing package to get the same outcome

install.packages("quantmod")
library("quantmod")
Delt(Adj)

hist(aapl_weekly_returns)

#The data looks similar to a bell-shaped distribution,
#We can see that it is unimodal (most probably has one mode, located in the interval from 0 to 0.5)
#The data is concentrated
#The weekly returns have mostly been positive (more observations are located from 0 to 0.15 rather than < 0)



