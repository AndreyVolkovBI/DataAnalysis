library(gmodels)
library(class)

n = 12 # length(Andrey Volkov) == 12
a <- round(runif(1, 0.4, 0.6), 1) # generate and round random distribution

# generates matrix of ${observation_count} observations
getX <- function (observationCount) {
  return (matrix(runif(n, 0, 1), observationCount, n))
}

# crates Y result vector for specific X matrix by specific rule
getY <- function(observations) {
  observationsCount <- length(observations[,1])
  Y <- matrix(0, observationsCount, 1)
  for(i in 1:observationsCount) { if (observations[i, 1] > a) { Y[i,1] <- 1 } }
  return (Y)
}

xTrain <- getX(80)
yTrain <- getY(xTrain)

xTest <- getX(100)
yTest <- getY(xTest)

# Check knn prediction with different k levels
getError <- function(k_num, xTrain_, xTest_, yTrain_, yTest_) {
  error <- matrix(0, k_num, 1)
  
  for (i in 1:k_num) {
    knnPred <- knn(xTrain_, xTest_, yTrain_, prob = T, k = i)
    probPred <- attr(knnPred, "prob")
    
    reg <- probPred * (as.numeric(knnPred) - 1) + (1 - probPred) * (1 - (as.numeric(knnPred) - 1))
    error[i] <- mean((reg - yTest_) ^ 2)
  }
  
  return (error)
}

k_num <- 30

errorTest <- getError(k_num = k_num, xTrain, xTest, yTrain, yTest)
plot(
  errorTest, type = "l", col = "blue", 
  xlab = "Number of nearest neighbors, k", ylab = "Test MSE", main = "Test MSE against the number of nearest neighbors"
)

errorTraining <- getError(k_num = k_num, xTrain, xTrain, yTrain, yTrain)
plot(
  errorTraining, type = "l", col = "green", 
  xlab = "Number of nearest neighbors, k", ylab = "Training MSE", main = "Training MSE against the number of nearest neighbor"
)

# f) Compute and plot the bias and variance against the nu,ber of nearest neighbours.

bias <- matrix(0, k_num, 1)
variance <- matrix(0, k_num, 1)

for (k in 1:k_num) {
  regression <- matrix(0, 100, 1)
  for(p in 1:70){
    xTrainSet <- matrix(runif(n, 0, 1), 80, n)
    yTrainSet <- matrix(0, 80, 1)
    for(i in 1:80) { if (xTrainSet[i, 1] > a) { yTrainSet[i, 1] <- 1 } }
    knnBiasVariance <- knn(xTrainSet, xTest, yTrainSet, prob = T, k = k)
    probBiasVariance <- attr(knnBiasVariance, "prob")
    regression <- regression + probBiasVariance * (as.numeric(knnBiasVariance) - 1) + (1 - probBiasVariance) * (1 - (as.numeric(knnBiasVariance) - 1))
  }
  averageKnn <- regression / 70
  
  variance[k] <- mean((averageKnn - regression) ^ 2)
  bias[k] <- mean((averageKnn - yTest) ^ 2)
}

plot(bias, type = 'l', col ='red', xlab = "Number of neighbors, k", ylab = "Bias", main = "Bias against the number of nearest neighbors")

plot(variance, type = 'l', col ='blue', xlab = "Number of neighbors, k", ylab = "Variance", main = "Variance against the number of nearest neighbors")

plot(bias, type = 'l', col = 'green', ylim = c(0,1), xlim = c(0,30), main = "Bias on top of test error graph")
lines(errorTest, col = 'blue')







