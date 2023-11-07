

setwd("/Users/seanphelan/Desktop/Regression")
stream = read.csv('streamflow.csv')
stream$STAID <- NULL

library(car)

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
