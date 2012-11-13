###################################################
# R code on a tutorial on PCA by Lindsay I Smith  #
###################################################

#1.get the data with two dimensions (x and y)
x<-c(2.5,0.5,2.2,1.9,3.1,2.3,2,1,1.5,1.1)
y<-c(2.4,0.7,2.9,2.2,3.0,2.7,1.6,1.1,1.6,0.9)
data <- cbind(x,y)
head(data)

#2.means substraction
adjustedData <- scale(data, scale=FALSE)
head(adjustedData)

#plot the data
plot(adjustedData)

#3.calculate the covariance matrix
covMat <- cov(adjustedData)

#4. compute eigen vector 
eigenMat <- eigen(covMat)
eigenVect <- eigenMat$vectors
eigenVal <- eigenMat$values

#check if the eigenvectors with the length of 1 (unit vector)
length_1vect <- sqrt(0.6778734^2+0.7351787^2)
length_2vect <- sqrt(0.7351787^2 + 0.6778734^2)

#plot the data with eigen vectors
plot(adjustedData, xlim=c(-2,2), ylim=c(-2,2), asp=1, frame=FALSE)
lines(adjustedData[,"x"], eigenVect[2,1]/eigenVect[1,1]*adjustedData[,"x"],
      lty = 1)
lines(adjustedData[,"x"], eigenVect[2,2]/eigenVect[1,2]*adjustedData[,"x"],
      lty = 3)
legend("topright",c('PC1', 'PC2'), lty=c(1,3), box.lwd=0)

#get the final data in terms of eigen vectors1 and 2
finaldata <- t(eigenVect)%*%t(adjustedData)
finaldata
plot(finaldata[1,], finaldata[2,], asp =1, xlim=c(-2,2), ylim=c(-2,2),
     xlab='eigenVector1', ylab='eigenVector2', frame=FALSE)
abline(h=0, lty=2)

#get the final data in term of only eigen vector1
finaldata_vect1 <- t(eigenVect[,1])%*%t(adjustedData)
finaldata_vect1

#plot before and after PCA transformation
par(mfrow=c(1,2))

#original data with two eigen vectors
plot(adjustedData, asp=1, xlim=c(-2,2), ylim=c(-2,2),
     xlab='x', ylab='y', frame=FALSE)
lines(adjustedData[,"x"], eigenVect[2,1]/eigenVect[1,1]*adjustedData[,"x"], 
      lty = 1)
lines(adjustedData[,"x"], eigenVect[2,2]/eigenVect[1,2]*adjustedData[,"x"], 
      lty = 3)
legend("topright",c('PC1', 'PC2'), lty=c(1,3), box.lwd=0)

# transfomed data
plot(finaldata[1,], finaldata[2,], asp =1, xlim=c(-2,2), ylim=c(-2,2),
     xlab='eigenVector1', ylab='eigenVector2', frame=FALSE)

#get the data (mean-adjusted) back using both vectors!
rowDataAdjust <- t(t(eigenVect)) %*% finaldata
orig_mean <- matrix(c(1.81,1.91))
rowDataAdjust_1 <- rowDataAdjust[1,]+1.81
rowDataAdjust_2 <- rowDataAdjust[2,]+1.91
rowDataAdjust <- rbind(rowDataAdjust_1, rowDataAdjust_2)
rowDataAdjust <- t(rowDataAdjust)

#get the data back using only the first eigen vector
rowDataAdjust_vect1 <- t(t(eigenVect[,1])) %*% finaldata_vect1
rowDataAdjust_vect1_1 <- rowDataAdjust_vect1[1,] + 1.81
rowDataAdjust_vect1_2 <- rowDataAdjust_vect1[1,] + 1.91
rowDataAdjust_vect1 <- rbind(rowDataAdjust_vect1_1, rowDataAdjust_vect1_2)
rowDataAdjust_vect1 <- t(rowDataAdjust_vect1)

#compare the data from original and the one with only from first eigen vector
par(mfrow=c(1,2))
plot(rowDataAdjust, asp =1, xlim=c(-4,4), ylim=c(-4,4),
     xlab='x', ylab='y', frame=FALSE)
title(main='Original Data')

plot(rowDataAdjust_vect1, asp =1, xlim=c(-4,4), ylim=c(-4,4),
     xlab='x', ylab='y', frame=FALSE)
title(main='Original Data from the first eigen vector')