#Write a code to construct CIs of given Confidence Level in R, for the Bernoulli Model
#parameter p, and for the Normal Model, parameter m.
#Say, for the Bernoulli case, you can write a function ci.bernoulli(x,a): the inputs are x -
#our Dataset, and a, the confidence level, and the output will be a CI for p in the Bernoulli(p)
#Model, for the given CL. The next function will be ci.normal.mu.givensig(x,sigma, a),
#which will calculate a CI of level a for Normal Model m in the case when s is known, based on the Data x.

ci.bernoulli <- function(x , a){

  n <- length(x)
  c(mean(x) - 1/(2*sqrt(n*a)), mean(x) + 1/(2*sqrt(n*a)))
  
}


ci.bernoulli(x1,a1)




ci.normal.mu.givensig <- function(x, sigma, a){

  n <- length(x)
  t <- qt(1-(a/2), df = n-1)
  s <- sd(x)
  m <- t*s/sqrt(n)
  c(mean(x)-m , mean(x)+m)
  

}

#example from Moodle 

x1 <- c(1.14453951390,  0.33225379148,  4.00416672357, -5.04850836904,  5.35766859766,
        0.06489233128, -5.94509889146, -4.03921700123, -1.04401996422, -1.24168000854,
        -8.06484372074,  1.03773667538, -2.87971666208, -0.71685156645,  0.45405920327,
        3.30095387831,  0.74257550749,  2.13158422402, -3.58842410744, -2.51157686643,
        -0.05550722588, -1.12422861053, -4.33075098368, -5.02908441414, -1.91539585567,
        1.00226791412, -0.23604245900, -4.60662137207, -3.35289234669, -3.41713159383,
        -6.73744315582, -2.45756382078, -1.15391641560,  1.50038591268,  3.66853942653,
        -1.05244315692,  4.30821758360, -0.99412513605,  3.02116764894,  3.14134780634,
        -1.06686798120, -5.78622173091, -1.38023764474, -1.87756991746,  5.05295459284,
        -0.62753127955,  0.31867113246, 3.79992961755, -0.07876558486, -5.08016167207)

sigma1 <- var(x1)

a1 <- 0.08

ci.normal.mu.givensig(x1,sigma1, a1)

