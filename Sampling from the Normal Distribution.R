#xi ~ N(100,16)
x<-rnorm(n=100,mean=3,sd=4)
x
x_bar <- (1/100)*sum(x)
x_bar
#x_bar ~ N(100,16/100)
x_bar_dist <- rnorm(n=100,mean=3,sd=.40)
x_bar_dist
par(mfrow=c(3,1))
hist(x,col="blue",pch=20,main="Histogram of Normal Deviates")
hist(x_bar_dist,col="green",pch=20,main="Histogram of Sample Mean",xlab="X_Bar")
mean(x)
Xi_diff_X_bar <- x - x_bar
Xi_diff_X_bar
Xi_diff_X_bar_square <- Xi_diff_X_bar^2
Xi_diff_X_bar_square
sum_square <- sum(Xi_diff_X_bar_square)
sum_square
S_2 <- sum_square/99
S_2
chi_square <- 99*S_2/16
chi_square
curve(dchisq(x, df = 99), from = 0, to = 150,lwd=3,col="purple",ylab="density",main="Distribution of (n-1)S^2/sigma^2 99 degrees of freedom")
#What is the probablity that (n-1)S^2/sigma^2 is greater than 70?
pchisq(70,df=99,lower.tail=FALSE)


x_norm <- rnorm(n=100,mean=0,sd=1)
hist(x_norm,col="green",main="Histogram of Standard Normal Deviates",xlab="x")
x_norm_squared <- x_norm^2
x_norm_squared
sum_x_norm_squared <- sum(x_norm_squared)
sum_x_norm_squared
hist(x_norm_squared,col="blue",main="Histogram of Squared Standard Normal Deviates",xlab="x^2")
#what is the probability that the sum of the squared standard normal variates will be greater than 124.34
curve(dchisq(x,df=100),from = 0, to = 150,lwd=3,col="purple",main="Chi Square Density Curve with 100 degrees of freeedom",ylab="density")
pchisq(124.34, df=100, lower.tail=FALSE)
#Answer: 5%
#what is the probability that the sum of the squared standard normal variates will be greater than 90
pchisq(90,df=100,lower.tail=FALSE)
#Anser: 75%