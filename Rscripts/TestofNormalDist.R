install.packages('demoKde')
library(demoKde)
x<- read.csv('F:\\Paper_shallowxdeep\\Density_occ\\Latitude_occ_deep.csv')
x <- x[,2]

install.packages('diptest')
library(diptest)
x<- read.csv('F:\\Paper_shallowxdeep\\Density_occ\\Latitude_occ_deep.csv')
x <- x[,2]

(dp <- dip.test(x))
plot(density(x)); rug(x) #p-value = 0.9345 alternative hypothesis : non-unimodal - at least bimodal 

s<- read.csv('F:\\Paper_shallowxdeep\\Density_occ\\Latitude_occ_Shallow.csv')
s <- s[,2]
(dp_s <- dip.test(s))
  plot(density(s)); rug(s) #p-value = 0.6709 alternative hypothesis : non-unimodal - at least bimodal 

  #Longitude
  x<- read.csv('D:\\Black_HD\\Paper_shallowxdeep\\Density_occ\\Longitude_occ_deep.csv')
  x <- x[,2]
  
  (dp <- dip.test(x))
  plot(density(x)); rug(x)   
  


## a first non-trivial case
(d.t <- dip.test(c(0,0, 1,1))) # "perfect bi-modal for n=4" --> p-value = 0
stopifnot(d.t$p.value == 0)

data(statfaculty)
plot(density(statfaculty)); rug(statfaculty)
(d.t <- dip.test(statfaculty))

x <- c(rnorm(50), rnorm(50) + 3)
plot(density(x)); rug(x)
## border-line bi-modal ...  BUT (most of the times) not significantly:
dip.test(x)
dip.test(x, simulate=TRUE, B=5000)

## really large n -- get a message
dip.test(runif(4e5))

(dp <- dip.test(x))
plot(density(x)); rug(x)




















kde(x, bw = bw.nrd0, kernel = kernelGaussian, n = 4096,
    from = min(x) , to = max(x),
    adjust = 1)

hist(x, xlab = "Deep-sea Occourence latitudinal distribution", ylab = "Frequency",
     probability = TRUE, main = "Gaussian kernel",
     border = "gray")
lines(density(x, width = 12), lwd = 1)
rug(x)
9 R> hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
          10 + probability = TRUE, main = "Rectangular kernel",
          11 + border = "gray")
12 R> lines(density(x, width = 12, window = "rectangular"), lwd = 2)
13 R> rug(x)
14 R> hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
           15 + probability = TRUE, main = "Triangular kernel",
           16 + border = "gray")
17 R> lines(density(x, width = 12, window = "triangular"), lwd = 2)
18 R> rug(x)






library("boot")
 fit <- function(x, indx) {
   a <- Mclust(x[indx], minG = 2, maxG = 2)$parameters
   if (a$pro[1] < 0.5)
     return(c(p = a$pro[1], mu1 = a$mean[1],
               opar <- as.list(opp$par)
               rx <- seq(from = 40, to = 110, by = 0.1)
               d1 <- dnorm(rx, mean = opar$mu1, sd = opar$sd1)
               d2 <- dnorm(rx, mean = opar$mu2, sd = opar$sd2)
               f <- opar$p * d1 + (1 - opar$p) * d2
               hist(x, probability = TRUE, xlab = "Waiting times (in min.)",
                       + border = "gray", xlim = range(rx), ylim = c(0, 0.06),
                       + main = "")
              lines(rx, f, lwd = 2)
               lines(rx, dnorm(rx, mean = mean(x), sd = sd(x)), lty = 2,
                        + lwd = 2)
               legend(50, 0.06, lty = 1:2, bty = "n",
                         + legend = c("Fitted two-component mixture density",
                                      + "Fitted single normal density"))