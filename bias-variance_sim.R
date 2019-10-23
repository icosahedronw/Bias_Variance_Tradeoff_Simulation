# bais variance trade-off  regression
getwd()
setwd("C:/Chanel/NYU/Data Science Translation")

# knn 

library(caret)

getError <- function(k,num,modeltype,seeds,n_test){
set.seed(seeds)


testset <- as.data.frame(matrix(runif(n_test*21,0,1),n_test))

Allfx_hat <- matrix(0,n_test,num)
Ally <- matrix(0,n_test,num)
Allfx <- matrix(0,n_test,num)

for (i in 1:num){
  trainset <- as.data.frame(matrix(runif(80*21,0,1),80))
  
  
  fx_train <- ifelse(trainset[,1]>0.5,1,0)
  trainset[,21] <- fx_train
  
  fx_test <- ifelse(testset[,1]>0.5,1,0)
  testset[,21] <- fx_test 
  
  
  # knn model
  knnmodel <- knnreg(trainset[,1:20],trainset[,21],k = k)
  probs <- predict(knnmodel, newdata = testset[,1:20])
  
  
  Allfx_hat[,i] <- probs
  Ally[,i] <- testset[,21]
  Allfx[,i] <- fx_test
  
} 


# irreducible <- sigma^2
irreducible  <- mean(apply( Allfx - Ally  ,1,var))
SquareBais  <- mean(apply((Allfx_hat - Allfx)^2,1,mean))
Variance <- mean(apply(Allfx_hat,1,var))

if (modeltype == 'reg'){
  
  PredictError  <- irreducible + SquareBais + Variance 

 }else{
  
  PredictError  <- mean(ifelse(Allfx_hat>=0.5,1,0)!=Allfx)
 }
  
 result <- data.frame(k,irreducible,SquareBais,Variance,PredictError)
 
 return(result)
}


# ----------------   plot square error  knn ----------------------------

n_test <- 100
modeltype <- 'reg'
num <- 100

seeds <- 1

result <- getError(2,num,modeltype,seeds,n_test)
result <- rbind(result,getError(5,num,modeltype,seeds,n_test))
result <- rbind(result,getError(7,num,modeltype,seeds,n_test))
for (i in seq(10,50,10)){
  result <- rbind(result,getError(i,num,modeltype,seeds,n_test))

}

result$PredictError
result$SquareBais
result$Variance

which.min(result$PredictError)
which.min(result$SquareBais)
which.min(result$Variance)


pdf(file = "k-NN Regression.pdf")

plot(result$k,sqrt(result$PredictError),type = 'o',col = 'brown',
      xlim = c(0,50),ylim = c(0,0.6),xlab = '', ylab ='', lwd = 3)
par(new = T)
plot(result$k,sqrt(result$SquareBais),type = 'o',col = '467',
      xlim = c(0,50),ylim = c(0,0.6),xlab = '', ylab ='', lwd = 3)
par(new = T) 
plot(result$k,sqrt(result$Variance),type = 'o',col = 'blueviolet',
      xlim = c(0,50),ylim = c(0,0.6),xlab = 'Number of Neighbors k', ylab ='Square Root of Component Error', lwd = 3,
      main = 'Bias-Variance Tradeoff in k-NN Regression')
abline(v=20,col="brown",lty=2,lwd=2)
abline(v=10,col="467",lty=2,lwd=2)
abline(v=50,col="blueviolet",lty=2,lwd=2)
legend("topright", "(x,y)",legend=c("Square Root of Test MSE", "Test Bias", "Test Standard Deviation"), col = c('brown','467','blueviolet'),lty=1,lwd=2,cex=.8)
dev.off()

# ----------------------  plot 0-1 loss knn -------------------------
modeltype <- 'classification'
num <- 100
n_test <- 100
seeds <- 1

result <- getError(2,num,modeltype,seeds,n_test)
result <- rbind(result,getError(5,num,modeltype,seeds,n_test))
result <- rbind(result,getError(7,num,modeltype,seeds,n_test))
for (i in seq(10,50,10)){
  result <- rbind(result,getError(i,num,modeltype,seeds,n_test))
  
}

result$PredictError
result$SquareBais
result$Variance
which.min(result$PredictError)
which.min(result$SquareBais)
which.min(result$Variance)

pdf(file = "k-NN Classification.pdf")

plot(result$k,sqrt(result$PredictError),type = 'o',col = 'brown',
      xlim = c(0,50),ylim = c(0,0.6),xlab = '', ylab ='', lwd = 3)
par(new = T)
plot(result$k,sqrt(result$SquareBais),type = 'o',col = '467',
      xlim = c(0,50),ylim = c(0,0.6),xlab = '', ylab ='', lwd = 3)
par(new = T) 
plot(result$k,sqrt(result$Variance),type = 'o',col = 'blueviolet',
      xlim = c(0,50),ylim = c(0,0.6),xlab = 'Number of Neighbors k', ylab ='Square Root of Component Error', lwd = 3,
      main = 'Bias-Variance Tradeoff in k-NN Classification')
legend("topright", "(x,y)",legend=c("Square Root of Test MSE", "Test Bias", "Test Standard Deviation"), col = c('brown','467','blueviolet'),lty=1,lwd=2,cex=.8)
abline(v=20,col="brown",lty=2,lwd=2)
abline(v=10,col="467",lty=2,lwd=2)
abline(v=50,col="blueviolet",lty=2,lwd=2)
dev.off()