library(dplyr)
library(ggplot2)
library(MASS)
library(e1071)
library(class)
library(mixtools)  #for ellipse


## Bivariate "banana" data 
## separable nonlinearly
library(readr)
banana <- read_csv("banana.csv", col_types = cols(X1 = col_skip()))
banana<-banana %>% mutate(class=ifelse(y==1,"class1","class2"))
ggplot(banana, aes(x=x1,y=x2, color=class))+geom_point()

## How to generate bivariate Normal distribution
#https://www.r-bloggers.com/simulating-from-the-bivariate-normal-distribution-in-r/
# The code is taken from the above blog

N <- 200 # Number of random samples
set.seed(123)
# Target parameters for univariate normal distributions
rho <- -0.6
mu1 <- 1; s1 <- 2
mu2 <- 1; s2 <- 8
# Parameters for bivariate normal distribution
mu <- c(mu1,mu2) # Mean 
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance matrix

# Function to draw ellipse for bivariate normal data
ellipse_bvn <- function(bvn, alpha){
  Xbar <- apply(bvn,2,mean)
  S <- cov(bvn)
  ellipse(Xbar, S, alpha = alpha, col="red")
}
#The way to go if you just want to get on with it, is to use the mvrnorm() function from the MASS package. 
bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma ) # from MASS package
plot(bvn1[,1],bvn1[,2],type="p",xlab="X1",ylab="X2")
ellipse_bvn(bvn1,.5)
ellipse_bvn(bvn1,.05)

## generate second class with different mean
bvn2 <- mvrnorm(N, mu = mu+c(10,10), Sigma = sigma ) # from MASS package
bvn<-rbind(bvn1,bvn2)
y<-c(rep(1,N),rep(-1,N))
bvn<-as.data.frame(cbind(bvn,y))
colnames(bvn)<-c("x1","x2","class")

bvn<-bvn %>% mutate(class=ifelse(class==1,"class1","class2"))
p<-ggplot(bvn, aes(x=x1,y=x2, color=class))+geom_point()

# Covariance matrix sigma
# compute its inverse
invsigma<-ginv(sigma)
m1<-colMeans(bvn1)
m2<-colMeans(bvn2)
md<-m1-m2
w=md %*% invsigma
## 
## Plot the lines
## w1*x1+w2*x2=0
slope=-w[1]/w[2]
p<-ggplot(bvn, aes(x=x1,y=x2, color=class))+geom_point()
p+geom_abline(intercept=0, slope=slope,color="green")

## We will model 
p+geom_abline(intercept=0, slope=slope,color="green")