#load iris dataset
data(iris)
data.iris <- iris
head(data.iris)

#pca on the go ...
iris.pca <- prcomp(iris[,1:4], scale.=TRUE)
par(mfrow=c(1,2))
plot(iris.pca)
biplot(iris.pca)

#plot score. and loadings.plots
par(mfrow=c(1,2))
#score.plot 
plot(iris.pca$x[,1], iris.pca$x[,2],xlab="PC1", ylab="PC2", pch=19, cex=0.8)
abline(h=0);abline(v=0)
#loading.plot
plot(iris.pca$rotation[,1], iris.pca$rotation[,2], xlab="PC1", ylab="PC2",
     pch=19, cex=0.8)
text(iris.pca$rotation, rownames(iris.pca$rotation), xpd=TRUE) 
abline(h=0); abline(v=0)

par(mfrow=c(1,2))
#score.plot with color label of species
plot(iris.pca$x[,1], iris.pca$x[,2],xlab="PC1", ylab="PC2",
     col=c("red","green","blue")[data.iris$Species], pch=19, cex=0.8)
legend("topright",pch=19,cex=0.8,col=c("red","green","blue"),
       legend = levels(data.iris$Species))
abline(h=0);abline(v=0)

#pattern detection
pc1_model <- predict(iris.pca)[,1]
plot(data.iris[,4], pc1_model,pch=19,cex=0.5,xlab="petal width", ylab="PC1")

#manual plotting
pairs(iris[,1:4], col=c("red","green","blue")[data.iris$Species],pch=19, cex=0.5)

