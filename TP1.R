data(iris)
dim(iris)
head(iris, 4)
names(iris)
attributes(iris)
summary(iris)
iris[1:5,]
plot(iris)
sepal_length <- iris$sepal.length
sepal_length
hist(sepal_length)
irisVer <- subset(iris, Species == "versicolor")
irisSet <- subset(iris, Species == "setosa")
irisVir <- subset(iris, Species == "virginica")
par(mfrow=c(1,3),mar=c(6,3,2,1))
boxplot(irisVer[,1:4], main="Versicolor, Rainbow Palette",ylim = c(0,8),las=2, col=rainbow(4))
boxplot(irisSet[,1:4], main="Setosa, Heat color Palette",ylim = c(0,8),las=2, col=heat.colors(4))
boxplot(irisVir[,1:4], main="Virginica, Topo colors Palette",ylim = c(0,8),las=2, col=topo.colors(4))
k = ?iris
k
pairs(iris[,1:4])
k = describe(iris) 
k
k = quantile(iris$Sepal.Length)
k
k["25%"]
