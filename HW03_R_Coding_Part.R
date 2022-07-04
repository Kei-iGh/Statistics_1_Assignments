#Problem 5
#Generate two random samples of size 1000 from two different Normal Distributions (i.e.,
#from Normal Distributions with different parameters). Merge (concatenate) these samples
#and plot the QQ-Plot of the obtained Dataset vs the Standard Normal Distribution.

x <- rnorm(1000, 5, 6.7)
y <- rnorm(1000, 2, 0.6)

z <- c(x, y)

qqplot(z, rnorm(1000, 0, 1))


#Problem 6
#Here we use the Animals Dataset from the MASS package. If you do not have that package, use install.packages("MASS") to install.
#The Dataset has two variables: body and brain, measuring the average brain and body weights for 28 species of land animals.

#. Calculate the Sample Correlation Coefficient between the body and brain weights for all animals. Give also the Scatterplot.
#. In the list of Animals, we have 3 Jurassic Period animals (3 Dinosaurs). Remove
#the observations for that Dinosaurs, and calculate the Sample Correlation Coefficient
#between the body and brain weights for all other animals. Give also the Scatterplot.

install.packages("MASS")


MASS::Animals$body

cor(MASS::Animals$body, MASS::Animals$brain)
plot(MASS::Animals$body, MASS::Animals$brain)



new_animals <- MASS::Animals[!(row.names(MASS::Animals) %in% c("Dipliodocus","Triceratops", "Brachiosaurus")), ]

cor(new_animals$body, new_animals$brain)
plot(new_animals$body, new_animals$brain)
