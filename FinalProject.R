
rm( list = ls())
setwd("/Users/seanphelan/Desktop/Regression")
stream = read.csv('streamflow.csv')
stream$STAID <- NULL

library('car')
library('pracma')
library('moments')
library('DescTools')
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

addVarPlotModel <- lm(data=stream,max90~DRAIN_SQKM+PPTAVG_BASIN+T_AVG_BASIN+T_AVG_SITE+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
avPlots(addVarPlotModel)

#  Basic LM of our project and leverage, fitted values, and residual plots
library('car')
n = 293
regstream <- lm(max90 ~ DRAIN_SQKM + PPTAVG_BASIN + T_AVG_BASIN + T_AVG_SITE + RH_BASIN +  MAR_PPT7100_CM + RRMEDIAN, stream)
regstream_matrix <- as.matrix(regstream)
Y <- matrix(stream[,1], ncol = 1)
X <- as.matrix(stream[,-1])
onevec <- matrix(rep(1,n), ncol = 1)
X <- cbind(onevec,X)
X <- as.matrix(X)
inv.XX <- solve(t(X)%*%X)
H <- X%*%inv.XX%*%t(X)
#  leverages
lev <- diag(H)
#  Residuals
res_stream <- rstudent(regstream)
#  fitted values
fittedvalues_stream <- fitted(regstream)

#  residual plot against time
par(mfrow=c(1,1))
plot(res_stream , xlab="time", ylab="residual", main="Plot of residual against time")

#  residual plots against fitted values:
plot(res_stream~fittedvalues_stream, xlab="fitted values", ylab="residual", 
     main="Plot of residuals against fitted values")

#  residual plots against predictor values: 
par(mfrow=c(2,4))
plot(res_stream~stream$DRAIN_SQKM, xlab="Violent predictor", ylab="residual", 
     main="Plot of residuals against Violent predictor")
plot(res_stream~stream$PPTAVG_BASIN, xlab="Violent predictor", ylab="residual", 
     main="Plot of residuals against Violent predictor")
plot(res_stream~stream$T_AVG_BASIN, xlab="Violent predictor", ylab="residual", 
     main="Plot of residuals against Violent predictor")
plot(res_stream~stream$T_AVG_SITE, xlab="Violent predictor", ylab="residual", 
     main="Plot of residuals against Violent predictor")
plot(res_stream~stream$RH_BASIN, xlab="Violent predictor", ylab="residual", 
     main="Plot of residuals against Violent predictor")
plot(res_stream~stream$MAR_PPT7100_CM, xlab="Violent predictor", ylab="residual", 
     main="Plot of residuals against Violent predictor")
plot(res_stream~stream$RRMEDIAN, xlab="Violent predictor", ylab="residual", 
     main="Plot of residuals against Violent predictor")
