

setwd("/Users/seanphelan/Desktop/Regression")
stream = read.csv('streamflow.csv')
stream$STAID <- NULL

library(car)
library(lme4)

######## exploratory data analysis #######
summary(stream)

par(mfrow=c(2,4))

hist(stream$max90,xlab = 'max90', main="Max 90th Percentile Flow") #worry
hist(stream$DRAIN_SQKM,xlab = 'DRAIN_SQKM', main="Drainage Area") #worry
hist(stream$PPTAVG_BASIN,xlab = 'PPTAVG_BASIN', main="Average Basin Precipitation") #worry
hist(stream$T_AVG_BASIN,xlab = 'T_AVG_BASIN', main="Average Basin Temperature")
hist(stream$T_AVG_SITE,xlab = 'T_AVG_SITE', main="Average Temperature at the Stream Location")
hist(stream$RH_BASIN,xlab = 'RH_BASIN', main="Average Relative Humidity Across the Basin") #worry
hist(stream$MAR_PPT7100_CM,xlab = 'MAR_PPT7100_CM', main="Average March Precipitation") #worry
hist(stream$RRMEDIAN,xlab = 'RRMEDIAN', main="Median Relief Ratio")

boxplot(stream$max90, main="Max 90th Percentile Flow") 
boxplot(stream$DRAIN_SQKM, main="Drainage Area") 
boxplot(stream$PPTAVG_BASIN, main="Average Basin Precipitation") 
boxplot(stream$T_AVG_BASIN, main="Average Basin Temperature")
boxplot(stream$T_AVG_SITE, main="Average Temperature at the Stream Location")
boxplot(stream$RH_BASIN, main="Average Relative Humidity Across the Basin")
boxplot(stream$MAR_PPT7100_CM, main="Average March Precipitation")
boxplot(stream$RRMEDIAN, main="Median Relief Ratio")

pairs(stream)

cor_stream = cor(stream)
cor_stream
#write.csv(cor_stream,"ProbMatrix.csv")

streamFirstModel <- lm(data=stream,max90~DRAIN_SQKM+PPTAVG_BASIN+T_AVG_BASIN+T_AVG_SITE+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
avPlots(addVarPlotModel)

###### all predictor linear model analysis

n=length(stream$max90)
p=8
alpha = .05

Y <- matrix(data=stream$max90,nrow = n,ncol = 1)
X <- as.matrix(stream[2:8])
X <- cbind(rep.int(1,n),X)
colnames(X)[1]<-"Intercept"

inv.XX <- solve(t(X)%*%X)
b <- inv.XX%*%t(X)%*%Y

I <- diag(1,n,n)
H <- X%*%inv.XX%*%t(X)
Yhat <- H%*%Y
e <- Y - Yhat

J <- matrix(rep.int(1,n*n),ncol=n,nrow = n)
SSE <- t(Y)%*%(I-H)%*%Y
MSE <- SSE/(n-p)

SST <- t(Y)%*%(I-(1/n)*J)%*%Y
SSR <- t(Y)%*%(H-(1/n)*J)%*%Y

Rsquared <- 1-(SSE/SST)
RsqAdj <- 1-((n-1)/(n-p))*(SSE/SST)

MSR <- SSR/(p-1)
Fstat <- MSR/MSE
CritF <- qf(.95,p-1,n-p)
pval <- 1-pf(Fstat,p-1,n-p)

cov.b <- inv.XX*c(MSE)
s.bhats <- sqrt(diag(cov.b))

t.values <- b/s.bhats
p.values <- 2*(1-pt(abs(t.values),n-p))
p.values
