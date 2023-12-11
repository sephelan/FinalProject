
rm( list = ls())
setwd("/Users/seanphelan/Desktop/Regression")
stream = read.csv('streamflow.csv')
stream$STAID <- NULL
library("qpcR")
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



##########multicolinearity#################
vif(streamFirstModel)
#yes, mostly avg basin and avg site with less in ppt avg basin
streamNOBasinModel <- lm(data = stream , max90~DRAIN_SQKM+PPTAVG_BASIN+T_AVG_SITE+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
streamNOSiteModel <- lm(data = stream , max90~DRAIN_SQKM+PPTAVG_BASIN+T_AVG_BASIN+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
streamNOavgbasinModel <- lm(data = stream , max90~DRAIN_SQKM+T_AVG_SITE+T_AVG_BASIN+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
streamNOmarpptModel <- lm(data = stream , max90~DRAIN_SQKM+PPTAVG_BASIN+T_AVG_SITE+T_AVG_BASIN+RH_BASIN+RRMEDIAN)
vif(streamNOSiteModel)
vif(streamNOBasinModel)
vif(streamNOavgbasinModel)
vif(streamNOmarpptModel)
streamavgsiteMARModel <- lm(data = stream , max90~DRAIN_SQKM+T_AVG_SITE+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
streamavgbasinMARodel <- lm(data = stream , max90~DRAIN_SQKM+T_AVG_BASIN+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
streamavgsitePPTModel <- lm(data = stream , max90~DRAIN_SQKM+T_AVG_SITE+RH_BASIN+PPTAVG_BASIN+RRMEDIAN)
streamavgbasinPPTodel <- lm(data = stream , max90~DRAIN_SQKM+T_AVG_BASIN+RH_BASIN+PPTAVG_BASIN+RRMEDIAN)

summary(streamavgsiteMARModel)
summary(streamavgbasinMARodel)

vif(streamavgbasinMARodel)
sum(vif(streamavgbasinMARodel))/5
vif(streamavgsiteMARModel)
sum(vif(streamavgsiteMARModel))/5
vif(streamavgsitePPTModel)
sum(vif(streamavgsitePPTModel))/5
vif(streamavgbasinPPTodel)
sum(vif(streamavgbasinPPTodel))/5

remove(streamavgbasinPPTodel)
remove(streamavgbasinMARodel)
remove(streamavgsiteMARModel)
remove(streamavgsitePPTModel)

streamCorFixModel <- lm(data = stream, max90~DRAIN_SQKM+T_AVG_SITE+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
summary(streamCorFixModel)
summary(streamFirstModel)


streamAllInteractModel <- lm(data = stream, max90~(DRAIN_SQKM+T_AVG_SITE+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)^2)
summary(streamAllInteractModel)




######stepwise testing for model####
Base<- lm(data= stream, max90 ~ DRAIN_SQKM:MAR_PPT7100_CM)
summary(Base)
ols_mallows_cp(Base, streamAllInteractModel)
AIC(Base)
BIC(Base)
Base_add1 <- lm(data= stream, max90 ~ DRAIN_SQKM:MAR_PPT7100_CM +  DRAIN_SQKM:T_AVG_SITE)
summary(Base_add1)
ols_mallows_cp(Base_add1, streamAllInteractModel)
AIC(Base_add1)
BIC(Base_add1)
Base_add2 <- lm(data= stream, max90 ~ DRAIN_SQKM:MAR_PPT7100_CM +  DRAIN_SQKM:T_AVG_SITE + T_AVG_SITE:RRMEDIAN)
summary(Base_add2)
ols_mallows_cp(Base_add2, streamAllInteractModel)
AIC(Base_add2)
BIC(Base_add2)
Base_add3 <- lm(data= stream, max90 ~  DRAIN_SQKM:MAR_PPT7100_CM +  DRAIN_SQKM:T_AVG_SITE + T_AVG_SITE:RRMEDIAN +DRAIN_SQKM:RRMEDIAN)
summary(Base_add3)
ols_mallows_cp(Base_add3, streamAllInteractModel)
AIC(Base_add3)
BIC(Base_add3)
Tentative_Model <- lm(data= stream, max90 ~ DRAIN_SQKM:MAR_PPT7100_CM +  DRAIN_SQKM:T_AVG_SITE + T_AVG_SITE:RRMEDIAN)
summary(Tentative_Model)
anova(Tentative_Model, finalModel)


########ols all step section #######


d <- ols_step_all_possible(streamAllInteractModel)
plot(d)

summary(lm(stream$max90 ~ stream$DRAIN_SQKM +stream$T_AVG_BASIN +stream$RH_BASIN +stream$DRAIN_SQKM:stream$MAR_PPT7100_CM))

summary(lm(stream$max90 ~ stream$DRAIN_SQKM +stream$T_AVG_SITE +stream$RH_BASIN +stream$DRAIN_SQKM:stream$MAR_PPT7100_CM))
AIC(lm(stream$max90 ~ stream$DRAIN_SQKM +stream$T_AVG_SITE +stream$RH_BASIN +stream$DRAIN_SQKM:stream$MAR_PPT7100_CM))
BIC(lm(stream$max90 ~ stream$DRAIN_SQKM +stream$T_AVG_SITE +stream$RH_BASIN +stream$DRAIN_SQKM:stream$MAR_PPT7100_CM))


model576 <- lm(data = stream , max90 ~ RH_BASIN+ DRAIN_SQKM:T_AVG_SITE+ DRAIN_SQKM:MAR_PPT7100_CM+ T_AVG_SITE:RRMEDIAN)
summary(model576)
summary(Tentative_Model)
finalModel<- lm(data = stream , max90 ~ RH_BASIN+ DRAIN_SQKM:T_AVG_SITE+ DRAIN_SQKM:MAR_PPT7100_CM+ T_AVG_SITE:RRMEDIAN)
stream.final <- data.frame(stream$max90,stream$RH_BASIN,stream$DRAIN_SQKM*stream$T_AVG_SITE,stream$DRAIN_SQKM*stream$MAR_PPT7100_CM,stream$T_AVG_SITE*stream$RRMEDIAN)
X.final <- cbind(1,as.matrix(stream.final[,c(2:5)]))


AIC(finalModel)
BIC(finalModel)
ols_mallows_cp(finalModel,streamAllInteractModel)


jacknifes <- rstudent(finalModel)


#### hypothesis test for model fitting###
anova(Tentative_Model, streamAllInteractModel)
 # normality test for full # 
qqnorm(finalModel$residuals)
shapiro.test(finalModel$residuals)
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

#boxcox.summary <- boxcox(streamCorFixModel,optimize = TRUE)
#lambda <- boxcox.summary$lambda

boxcox.summary <- boxcox(finalModel, optimize = TRUE)
lambda <- boxcox.summary$lambda


#transfinalmodel <- lm((stream$max90)^lambda ~stream$DRAIN_SQKM+stream$MAR_PPT7100_CM+stream$T_AVG_BASIN)
#res_tran <- transfinalmodel$residuals
#jacknifes.trans <- rstudent((transfinalmodel))
#summary(transfinalmodel)
#qqnorm(res_tran)
#qqline(res_tran)
#ks.test(jacknifes.trans, 'pnorm', 0 ,1)

 
#  pvalue = 2.598e-08 so we have evidence to say the reduced model is a better fit. 

############ BP test #############
# Assumptions needed: Independence and normality with respect to error terms. 
# it also assumed variances of error terms are related ,??? how to test
# H0: y1 = 0 or the variances are constant. Ha: y1=/=0, the the assumption of constant variances is violated. 
# we know the normality assumption good, 
bptest(streamFirstModel)
# p-value = 1.505e-08, so we have enough evidence to reject ho, non constant variances. 
bptest(streamCorFixModel)
# p-value = 0.00317 also no constant variances. 

######### wls weighted regression ##########
weights <- 1/(finalModel$fitted.values)^2
W <- diag(weights)
inv.XWX <- solve(t(X.final)%*%W%*%X.final)
XWY <- t(X.final)%*%W%*%Y
b.w<-inv.XWX%*%XWY
b.w
b.sd = sqrt(diag(inv.XWX))
b.sd

weightedModel <- lm(data = stream , max90 ~ RH_BASIN+ DRAIN_SQKM:T_AVG_SITE+ DRAIN_SQKM:MAR_PPT7100_CM+ T_AVG_SITE:RRMEDIAN, weights = weights)
summary(weightedModel)

summary(finalModel)

AIC(weightedModel)



######## Box COX Transformations  ##########
#  basically from today we should get our assumptions safe first before choosing difference model fits, which mean the box cox transformation
#logmodel <- lm(log(max90) ~ DRAIN_SQKM + PPTAVG_BASIN + T_AVG_BASIN + T_AVG_SITE + RH_BASIN +  MAR_PPT7100_CM + RRMEDIAN, stream)
#res_logmodel <- residuals(logmodel)
#fit_log <- fitted.values(logmodel)
#plot(res_logmodel~fit_log)
#qqnorm(res_logmodel)
#qqline(res_logmodel)
#shapiro.test(res_logmodel)
#summary(regstream)

boxcox.summary.weighted <- boxcox(weightedModel, optimize = TRUE)
lambda.weighted <- boxcox.summary$lambda

lambdamodel <- lm(data = stream , max90^lambda.weighted ~ RH_BASIN+ DRAIN_SQKM:T_AVG_SITE+ DRAIN_SQKM:MAR_PPT7100_CM+ T_AVG_SITE:RRMEDIAN,weights = weights)
summary(lambdamodel)

AIC(lambdamodel)
BIC(lambdamodel)
ols_mallows_cp(lambdamodel,finalModel)
AIC(finalModel)
BIC(finalModel)


