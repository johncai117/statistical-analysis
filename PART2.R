rm(list = ls())
library(e1071) # For svm
library(Rsafd)
library(keras)
library(dplyr)



load(paste("/Users/johncai/Documents/FIN\ 505/FINAL_SUBMISSION/Final_project/", "Jan2020FinalData.r", sep =""))

## Question 1
X= x_train[2004,1,]
Y = x_train[2004,2,]

mean_X = mean(X)

plot(X)
abline(h = mean_X)

mean_Y = mean(Y)

plot(Y)
abline(h = mean_Y)

##Remove the padded zeros
NZ1 <- (X != 0 & Y != 0) 
X <- X[NZ1]
Y <- Y[NZ1]


DeltaX <- as.numeric(length(X) -1)
DeltaY <- as.numeric(length(X) -1)

for (i in seq(2,length(X))){ ## starts from 2
  DeltaX[i-1] <- X[i] - X[i-1]
}

for (i in seq(2,length(Y))){
  DeltaY[i-1] <- Y[i] - Y[i-1]
}


## Question 2

NZ <- (DeltaX != 0 & DeltaY != 0) 
DeltaX <- DeltaX[NZ]
DeltaY <- DeltaY[NZ]

DeltaX_s <- DeltaX
DeltaY_s <- DeltaY


## QQ Plots
qqnorm(DeltaX)
qqnorm(DeltaY)

## shape plots
shape.plot(DeltaX,tail="two")
shape.plot(DeltaY,tail="upper") ## deltaY only has upper tail

DeltaX.est <- fit.gpd(DeltaX, tail = "two", upper = 2000, lower = -2500)
DeltaY.est <- fit.gpd(DeltaY, tail = "upper", upper = 60)

DeltaX.est@upper.par.ests
DeltaX.est@lower.par.ests


tailplot(DeltaX.est)
tailplot(DeltaY.est)

### Q3 part 1

## find the quantiles of the distribution first

X_q = quantile(DeltaX, probs = 0.95)
X_q
Y_q = quantile(DeltaY, probs = 0.95)
Y_q

## empirical quantile method
count_q = 0
for (i in seq(1, length(DeltaX))){
  temp_X = DeltaX[i]
  temp_Y = DeltaY[i]
  if (temp_X > X_q & temp_Y > Y_q){
    count_q = count_q + 1
  }
}
count_q
count_q = count_q / length(DeltaX)
count_q


### simulation method
set.seed(15)
N <- 20000
DeltaX.sim <- rgpd(DeltaX.est,N) 
DeltaY.sim <- rgpd(DeltaY.est,N) 

count_q_sim = 0
for (i in seq(1, N)){
  temp_X = DeltaX.sim[i]
  temp_Y = DeltaY.sim[i]
  if (temp_X > X_q & temp_Y > Y_q){
    count_q_sim = count_q_sim + 1
  }
}
count_q_sim
count_q_sim = count_q_sim / length(DeltaX.sim)
count_q_sim


## part 3.2

X_q2 = 15000
Y_q2 = 200

count_q_sim2 = 0
for (i in seq(1, length(DeltaX.sim))){
  temp_X = DeltaX.sim[i]
  temp_Y = DeltaY.sim[i]
  if (temp_X > X_q2 & temp_Y > Y_q2){
    count_q_sim = count_q_sim + 1
  }
}
count_q_sim2
count_q_sim2 = count_q_sim2 / length(DeltaX.sim)
count_q_sim2

## I get a probability of 0. We are not cpaturing the covariance between the variables


##Finding the empirical copula
U <- pgpd(DeltaX.est, DeltaX)
V <- pgpd(DeltaY.est, DeltaY)
plot(U,V)
EMPCOP <- empirical.copula(U,V)
FAM <- "gumbel" ##Has upper tail dependence
ESTC <- fit.copula(EMPCOP,FAM)


## KENDALL AND SPEARMAN
Kendalls.tau(EMPCOP)
Kendalls.tau(ESTC)

Spearmans.rho(EMPCOP)
Spearmans.rho(ESTC)

## Genertaing values

N <- 200000
SD <- rcopula(ESTC,N)
Xsim <- qgpd(DeltaX.est, SD$x)
Ysim <- qgpd(DeltaY.est, SD$y)

count_q_sim3 = 0
for (i in seq(1, length(Xsim))){
  temp_X = Xsim[i]
  temp_Y = Ysim[i]
  if (temp_X > X_q2 & temp_Y > Y_q2){
    count_q_sim3 = count_q_sim3 + 1
  }
}
count_q_sim3
count_q_sim3 = count_q_sim3 / length(Xsim)
count_q_sim3



