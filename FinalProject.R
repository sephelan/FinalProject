
rm( list = ls())
setwd("/Users/seanphelan/Desktop/Regression")
stream = read.csv('streamflow.csv')
streamog = read.csv('streamflow.csv')
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
res_stream <- streamFirstModel$residuals
#write.csv(cor_stream,"ProbMatrix.csv")

addVarPlotModel <- lm(data=stream,max90~DRAIN_SQKM+PPTAVG_BASIN+T_AVG_BASIN+T_AVG_SITE+RH_BASIN+MAR_PPT7100_CM+RRMEDIAN)
avPlots(addVarPlotModel)

###### all predictor linear model analysis ###### 

n=length(stream$max90)
p=8
alpha = .05

Y <- matrix(data=stream$max90,ncol = 1)
X <- as.matrix(stream[2:8])
X <- cbind(rep.int(1,n),X)
colnames(X)[1]<-"Intercept"


# ###### adding in residual plots and all the df tests ###
plot(res_stream~ stream$max90,xlab="Response Variable",ylab="Residual", 
     main="Plot of residuals against Response Variable")
summary(streamFirstModel)
plot(streamFirstModel)
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

remove(streamNOSiteModel)
remove(streamNOBasinModel)
remove(streamNOavgbasinModel)
remove(streamNOmarpptModel)
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
anova(Tentative_Model, Base)


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
anova(finalModel, streamAllInteractModel)



######## outlier detection and testing#####

ols_plot_dffits(finalModel)
ols_plot_cooksd_chart(finalModel)
ols_plot_dfbetas(finalModel)

stream <- stream[stream$STAID !=2315500, ] #89
stream
finalmodelwo89 <- lm(data = stream , max90 ~ RH_BASIN+ DRAIN_SQKM:T_AVG_SITE+ DRAIN_SQKM:MAR_PPT7100_CM+ T_AVG_SITE:RRMEDIAN)
summary(finalmodelwo89)
ols_mallows_cp(finalmodelwo89, streamAllInteractModel)
AIC(finalmodelwo89)
BIC(finalmodelwo89)

stream <- stream[stream$STAID !=11532500, ] # 249 
finalmodelwo89_249 <- lm(data = stream , max90 ~ RH_BASIN+ DRAIN_SQKM:T_AVG_SITE+ DRAIN_SQKM:MAR_PPT7100_CM+ T_AVG_SITE:RRMEDIAN)
summary(finalmodelwo89_249)
ols_mallows_cp(finalmodelwo89_249, streamAllInteractModel)
AIC(finalmodelwo89_249)
BIC(finalmodelwo89_249)

stream <- stream[stream$STAID !=6191500, ] #164
finalmodelwo89_249_164 <- lm(data = stream , max90 ~ RH_BASIN+ DRAIN_SQKM:T_AVG_SITE+ DRAIN_SQKM:MAR_PPT7100_CM+ T_AVG_SITE:RRMEDIAN)
summary(finalmodelwo89_249_164)
ols_mallows_cp(finalmodelwo89_249_164, streamAllInteractModel)
AIC(finalmodelwo89_249_164)
BIC(finalmodelwo89_249_164)

stream <- stream[stream$STAID !=6452000, ] #179
finalmodelwo89_249_164_179 <- lm(data = stream , max90 ~ RH_BASIN+ DRAIN_SQKM:T_AVG_SITE+ DRAIN_SQKM:MAR_PPT7100_CM+ T_AVG_SITE:RRMEDIAN)
summary(finalmodelwo89_249_164_179)
ols_mallows_cp(finalmodelwo89_249_164_179, streamAllInteractModel)
AIC(finalmodelwo89_249_164_179)
BIC(finalmodelwo89_249_164_179)

stream <- stream[stream$STAID !=7056000, ] #194
finalmodelwo89_249_164_179_194 <- lm(data = stream , max90 ~ RH_BASIN+ DRAIN_SQKM:T_AVG_SITE+ DRAIN_SQKM:MAR_PPT7100_CM+ T_AVG_SITE:RRMEDIAN)
summary(finalmodelwo89_249_164_179_194)
ols_mallows_cp(finalmodelwo89_249_164_179_194, streamAllInteractModel)
AIC(finalmodelwo89_249_164_179_194)
BIC(finalmodelwo89_249_164_179_194)


####### outlier removal based on plot #######
#stream <- stream[stream$STAID !=6452000, ] #179 -Above
#stream <- stream[stream$STAID !=6191500, ] #164 -Above
#stream <- stream[stream$STAID !=11532500, ] # 249  -Above
#stream <- stream[stream$STAID !=2315500, ] #89 - Above
stream <- stream[stream$STAID !=2110500, ] #78- 
stream <- stream[stream$STAID !=2314500, ] #88- 
stream <- stream[stream$STAID !=6447000, ] #177 -
stream <- stream[stream$STAID !=3281500, ] #115 -
#stream <- stream[stream$STAID !=7056000, ] #194 -Above
stream <- stream[stream$STAID !=11468000, ] #244 -
stream <- stream[stream$STAID !=7067000, ] #197 -
finalmodel_nolier <- lm(data = stream , max90 ~ RH_BASIN+ DRAIN_SQKM:T_AVG_SITE+ DRAIN_SQKM:MAR_PPT7100_CM+ T_AVG_SITE:RRMEDIAN)
res_final_out<- rstudent(finalmodel_nolier)
fit_final_out <- fitted(finalmodel_nolier)
plot(res_final_out)
axis(2, at = seq(-3, 3, by = 1))
text(res_final_out, labels = stream$STAID, pos = 3)
summary(finalmodel_nolier)


# normality test # 
jacknifes <- rstudent(finalmodel_nolier)
stream.final <- data.frame(stream$max90,stream$RH_BASIN,stream$DRAIN_SQKM*stream$T_AVG_SITE,stream$DRAIN_SQKM*stream$MAR_PPT7100_CM,stream$T_AVG_SITE*stream$RRMEDIAN)


# normality test for full # 
res_final_out<- rstudent(finalmodel_nolier)
fit_final_out <- fitted(finalmodel_nolier)
qqnorm(res_final_out)
qqline(res_final_out)
shapiro.test(res_final_out)
ks.test(res_final_out, "pnorm", 0 ,1)


anova(finalmodel_nolier, streamAllInteractModel)
 # normality test for full # 
qqnorm(finalmodel_nolier$residuals)

shapiro.test(finalmodel_nolier$residuals)

ks.test(jacknifes,'pnorm',0,1)

##### Independence #####
plot(res_final_out, fit_final_out, xlab="Residuals", ylab="Fitted Values" , main="Residuals vs Fitted Values")

#  constant variances #

plot(fit_final_out, res_final_out, xlab="Residuals", ylab="Fitted Values" , main="Residuals vs Fitted Values")
par(mfrow=c(2,4))
plot(stream$DRAIN_SQKM, res_final_out, xlab="Residuals", ylab="X value" , main="Residuals vs DrainSQKM")
plot(stream$PPTAVG_BASIN, res_final_out, xlab="Residuals", ylab="X value" , main="Residuals vs PPTAVG_BASIN")
plot(stream$T_AVG_BASIN , res_final_out, xlab="Residuals", ylab="X value" , main="Residuals vs T_AVG_BASIN")
plot(stream$T_AVG_SITE, res_final_out, xlab="Residuals", ylab="X value" , main="Residuals vs T_AVG_SITE")
plot(stream$RH_BASIN , res_final_out, xlab="Residuals", ylab="X value" , main="Residuals vs RH_BASIN")
plot(stream$MAR_PPT7100_CM , res_final_out, xlab="Residuals", ylab="X value" , main="Residuals vs MAR_PPT7100_CM")
plot(stream$RRMEDIAN , res_final_out, xlab="Residuals", ylab="X value" , main="Residuals vs RRMEDIAN")

#  constant variances test for full #

boxcox.summary <- boxcox(finalModel, optimize = TRUE)
lambda <- boxcox.summary$lambda

############ BP test #############
# Assumptions needed: Independence and normality with respect to error terms. 
# it also assumed variances of error terms are related ,??? how to test
# H0: y1 = 0 or the variances are constant. Ha: y1=/=0, the the assumption of constant variances is violated. 
# we know the normality assumption good, 
bptest(streamFirstModel)
# p-value = 1.505e-08, so we have enough evidence to reject ho, non constant variances. 
bptest(streamCorFixModel)
# p-value = 0.00317 also no constant variances. 


######## Box COX Transformations  ##########


boxcox.summary <- boxcox(finalmodel_nolier, optimize = TRUE)
lambda.weighted <- boxcox.summary$lambda
lambdamodel <- lm(data = stream , max90^lambda.weighted ~ RH_BASIN+ DRAIN_SQKM:T_AVG_SITE+ DRAIN_SQKM:MAR_PPT7100_CM+ T_AVG_SITE:RRMEDIAN)




res_final <- rstudent(lambdamodel)
fit_final <- fitted.values((lambdamodel))
qqnorm(res_final)
ks.test(res_final, "pnorm", 0 ,1)
plot(res_final, fit_final)
abline(h=0)
plot(res_final)
par(mfrow=c(1,1))
par(mfrow=c(2,4))
plot(stream$DRAIN_SQKM, res_final, xlab="Residuals", ylab="X value" , main="Residuals vs DrainSQKM")
plot(stream$PPTAVG_BASIN, res_final, xlab="Residuals", ylab="X value" , main="Residuals vs PPTAVG_BASIN")
plot(stream$T_AVG_BASIN , res_final, xlab="Residuals", ylab="X value" , main="Residuals vs T_AVG_BASIN")
plot(stream$T_AVG_SITE, res_final, xlab="Residuals", ylab="X value" , main="Residuals vs T_AVG_SITE")
plot(stream$RH_BASIN , res_final, xlab="Residuals", ylab="X value" , main="Residuals vs RH_BASIN")
plot(stream$MAR_PPT7100_CM , res_final, xlab="Residuals", ylab="X value" , main="Residuals vs MAR_PPT7100_CM")
plot(stream$RRMEDIAN , res_final, xlab="Residuals", ylab="X value" , main="Residuals vs RRMEDIAN")
