
rm( list = ls())
setwd("/Users/seanphelan/Desktop/Regression")
stream = read.csv('streamflow.csv')
stream$STAID <- NULL

library('car')
library('pracma')
library('moments')
library('DescTools')
library('olsrr')
library("caret")
library("HH")
library("leaps")
library("StepReg")
library('lmtest')
library('EnvStats')
library('lawstat')
library('corrplot')
library('tidyverse')
library('e1071')
install.packages('e1071')
######## exploratory data analysis #######
summary(stream)
streamFirstModel <- lm(data=stream,max90~DRAIN_SQKM+PPTAVG_BASIN+T_AVG_BASIN+T_AVG_SITE+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)

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
res_stream <- streamFirstModel$residuals
jacknifes  <- rstudent(regstream)
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
plot(res_stream~stream$DRAIN_SQKM, xlab="Drain Area", ylab="residual", 
     main="Plot of residuals against Drain Area")
plot(res_stream~stream$PPTAVG_BASIN, xlab="Basin-Averaged Precipitation", ylab="residual", 
     main="Plot of residuals against Basin-Averaged Precipitation")
plot(res_stream~stream$T_AVG_BASIN, xlab="Basin-Averaged Temperature ", ylab="residual", 
     main="Plot of residuals against Basin-Averaged Temperature ")
plot(res_stream~stream$T_AVG_SITE, xlab="At-Site Temperature", ylab="residual", 
     main="Plot of residuals against At-Site Temperature")
plot(res_stream~stream$RH_BASIN, xlab="Basin-Averaged Relative Humidity", ylab="residual", 
     main="Plot of residuals against Basin-Averaged Relative Humidity")
plot(res_stream~stream$MAR_PPT7100_CM, xlab="Median Relief Ratio", ylab="residual", 
     main="Plot of residuals against Median Relief Ratio")
plot(res_stream~stream$RRMEDIAN, xlab="Average March Precipitatio", ylab="residual", 
     main="Plot of residuals against Average March Precipitatio")

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

avPlots(addVarPlotModel)

###### all predictor linear model analysis ###### 

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

# ###### adding in residual plots and all the df tests ###
plot(res_stream~ stream$max90,xlab="Response Variable",ylab="Residual", 
     main="Plot of residuals against Response Variable")
ols_plot_dffits(regstream)
ols_plot_cooksd_chart(regstream)
ols_plot_dfbetas(regstream)
summary(regstream)
plot(regstream)
table(stream$max90)





######### Model Fitting ########
summary(streamFirstModel)
plot(streamFirstModel)
streamReducedModel <- lm(stream$max90~stream$DRAIN_SQKM)
summary(streamReducedModel)
res_reducedmodel <- residuals(streamReducedModel)



##########multicolinearity#################
vif(streamFirstModel)
#yes, mostly avg basin and avg site with less in ppt avg basin
streamNOBasinModel <- lm(data = stream , max90~DRAIN_SQKM+PPTAVG_BASIN+T_AVG_SITE+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
streamNOSiteModel <- lm(data = stream , max90~DRAIN_SQKM+PPTAVG_BASIN+T_AVG_BASIN+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
streamNOavgbasinModel <- lm(data = stream , max90~DRAIN_SQKM+T_AVG_SITE+T_AVG_BASIN+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
vif(streamNOSiteModel)
vif(streamNOBasinModel)
vif(streamNOavgbasinModel)
streamavgsiteModel <- lm(data = stream , max90~DRAIN_SQKM+T_AVG_SITE+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
streamavgbasinodel <- lm(data = stream , max90~DRAIN_SQKM+T_AVG_BASIN+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
streampptavgModel <- lm(data = stream , max90~DRAIN_SQKM+PPTAVG_BASIN+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
streamavgbasinodel
sum(1-1/vif(streamavgbasinodel))
streamavgsiteModel
sum(1-1/vif(streamavgsiteModel))
vif(streampptavgModel)
anova(streamCorFixModel,streamFirstModel)
#now we have r2=.487 > r2=.465, meaning we will use our model streamavgbasinmodel
streamCorFixModel <- lm(stream$max90~stream$DRAIN_SQKM+stream$T_AVG_BASIN+stream$RH_BASIN+stream$MAR_PPT7100_CM+stream$RRMEDIAN)

res_corfix <- streamCorFixModel$residuals
jacknifes <- rstudent(streamCorFixModel)


 # normality test for full # 
qqnorm(res_corfix)
qqline(res_corfix)
shapiro.test(res_corfix)
ks.test(jacknifes,'pnorm',0,1)
#ks test says not normal p-value = 2.942e-05 and ks better than shapiro because n>50.

#  constant variances test for full #
################## Lack of fit test template and example for just x90 #####
# Lack of fit test aims to see if our fitted model is a more optimal model compared to the full model
# Needs assumptions of independence, constant variance, and normality with respect to the error terms.
#  also needs repeated observations, which we know we have. 
#  we know that 
# ? do we need to check assumptions for both reduced and full? 
# anova(reduced.lmfit,full.lmfit) lack of fit test
# h0: the full model is a better fit. ha: the reduced model is a better fit.
anova(streamReducedModel,streamCorFixModel)
summary(streamReducedModel)
summary(lm(stream$max90~stream$DRAIN_SQKM+stream$MAR_PPT7100_CM))
summary(lm(stream$max90~stream$DRAIN_SQKM+stream$MAR_PPT7100_CM+stream$T_AVG_BASIN))
summary(lm(stream$max90~stream$DRAIN_SQKM+stream$MAR_PPT7100_CM+stream$T_AVG_BASIN+stream$RH_BASIN))
streamFinalModel <- lm(stream$max90~stream$DRAIN_SQKM+stream$MAR_PPT7100_CM+stream$T_AVG_BASIN)

boxcox.summary <- boxcox(streamFinalModel,optimize = TRUE)
lambda <- boxcox.summary$lambda

transfinalmodel <- lm((stream$max90)^lambda ~stream$DRAIN_SQKM+stream$MAR_PPT7100_CM+stream$T_AVG_BASIN)
res_tran <- transfinalmodel$residuals
jacknifes.trans <- rstudent((transfinalmodel))
summary(transfinalmodel)
qqnorm(res_tran)
qqline(res_tran)
ks.test(jacknifes.trans, 'pnorm', 0 ,1)

 
#  pvalue = 2.598e-08 so we have evidence to say the reduced model is a better fit. 

############ BP test #############
# Assumptions needed: Independence and normality with respect to error terms. 
# it also assumed variances of error terms are related ,??? how to test
# H0: y1 = 0 or the variances are constant. Ha: y1=/=0, the the assumption of constant variances is violated. 
# we know the normality assumption good, 
bptest(streamFirstModel)
# p-value = 1.505e-08, so we have enough evidence to reject ho, non constant variances. 
bptest(streamReducedModel)
# p-value = 0.00317 also no constant variances. 

######## Box COX Transformations  ##########
#  basically from today we should get our assumptions safe first before choosing difference model fits, which mean the box cox transformation
logmodel <- lm(log(max90) ~ DRAIN_SQKM + PPTAVG_BASIN + T_AVG_BASIN + T_AVG_SITE + RH_BASIN +  MAR_PPT7100_CM + RRMEDIAN, stream)
res_logmodel <- residuals(logmodel)
fit_log <- fitted.values(logmodel)
plot(res_logmodel~fit_log)
qqnorm(res_logmodel)
qqline(res_logmodel)
shapiro.test(res_logmodel)
summary(regstream)
 b <- ols_step_all_possible(regstream)
plot(b) 
lambdamodel <- lm(max90^lambda ~ DRAIN_SQKM + PPTAVG_BASIN + T_AVG_BASIN + T_AVG_SITE + RH_BASIN +  MAR_PPT7100_CM + RRMEDIAN, stream)
summary(transfinalmodel)
vif(lambdamodel)

