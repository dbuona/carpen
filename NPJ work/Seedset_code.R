#This uses dataset with only 1 fixed effect (Treatment)
##This is the script used in the publication
rm(list=ls()) 
seedset=read.csv(file="seedsetdata.csv", h=T)
attach(seedset)
library(nlme)
#Fit a model with "loaded Mean Structure"
model3.1fit<-lme(seed_set~Treatment, random=~1|clone, seedset, method= "REML")
summary(model3.1fit)
anova(model3.1fit)
random.effects(model3.1fit)
#structure for Random Effect
model3.1Afit<-gls(seed_set~Treatment, data=seedset)
anova(model3.1fit,model3.1Afit)
#to get te correct pvalue for model 3.1 must divide by 2 (page. 80)
0.0365/2
# so pvalue is 0.01825 and therfore we must keep random effect in all subsequent models
#Model 3.2A seperate residual variance for each treatment
model3.2Afit<-lme(seed_set~Treatment, random=~1|clone,seedset, method="REML", weights=varIdent(form=~1|Treatment))
summary(model3.2Afit)
#test hypothesis 3.2
anova(model3.1fit,model3.2Afit)
# because p-value is not significant, should use homogeneous varience model
#final model is 3.1fit
model3.1fit<-lme(seed_set~Treatment, random=~1|clone, seedset, method= "REML")
summary(model3.1fit)
anova(model3.1fit)
TukeyHSD(aov(seed_set~Treatment, data=seedset))
#Test assumptions of LMM
par(mfrow=c(1,1))
plot(model3.1fit, main="untransformed_resid")
qqnorm(resid(model3.1fit),main="untransformedQQ")
shapiro.test(model3.1fit$residuals)
#it seems to fail assumptions of lmm
#LN Transfromation
LN_SEEDSET <- log(seed_set)
seedset$LN_SEEDSET <- with(seedset, LN_SEEDSET)
modellog<-lme(LN_SEEDSET~Treatment, random=~1|clone, seedset, method= "REML")
summary(modellog)
plot(modellog, main="LN.Residuals")
qqnorm(resid(modellog), main="LNQQ")
shapiro.test(modellog$residuals)
#square root transformation
SQRT_SEEDSET<-sqrt(seed_set)
seedset$SQRT_SEEDSET <- with(seedset, SQRT_SEEDSET)
modelsqrt<-lme(SQRT_SEEDSET~Treatment, random=~1|clone, seedset, method= "REML")
summary(modelsqrt)
plot(modelsqrt, main="SqRt.Residuals")
qqnorm(resid(modelsqrt), main="SqRtQQ")
shapiro.test(modelsqrt$residuals)
#Log10
LOG10<-log10(seed_set)
seedset$LOG10<-with(seedset, LOG10)
modellog10<-lme(LOG10~Treatment, random=~1|clone, seedset, method= "REML")
summary(modellog10)
plot(modellog, main="log10.residuals")
qqnorm(resid(modellog10), main="log10QQ")
shapiro.test(modellog10$residuals)
#No transformations seem to help with normality
boxplot(seed_set~Treatment, main= "Seed Set Results", ylab="Seed Set", xlab="Treatment")
####model for seedweight
model4.1fit<-lme(ave_seed_weight~Treatment, random=~1|clone, seedset, method= "REML")
summary(model4.1fit)
anova(model4.1fit)
random.effects(model4.1fit)
#structure for Random Effect
model4.1Afit<-gls(ave_seed_weight~Treatment, data=seedset)
anova(model4.1fit,model4.1Afit)
.8015/2
## random effect is not significant, therefore we can eliminate it??
linearweight<-lm(ave_seed_weight~Treatment)
summary(linearweight)
###fail to reject hypothesis
plot(model4.1fit, main="untransformed_resid")
qqnorm(resid(model4.1fit),main="untransformedQQ")
shapiro.test(model4.1fit$residuals)
boxplot(ave_seed_weight~Treatment, main="Seed Weight Results", ylab="Average Seed Weight (mg)", xlab="Treatment")
###transformation LN
LN_SEEDWEIGHT <- log(ave_seed_weight)
seedset$LN_SEEDWEIGHT <- with(seedset, LN_SEEDWEIGHT)
modellog2<-lme(LN_SEEDWEIGHT~Treatment, random=~1|clone, seedset, method= "REML")
summary(modellog2)
plot(modellog2, main="WEIGHT LN.Residuals")
qqnorm(resid(modellog2), main="WEIGHTLNQQ")
shapiro.test(modellog2$residuals)
###Squareroot
SQRT_SEEDWEIGHT<-sqrt(ave_seed_weight)
seedset$SQRT_SEEDWEIGHT <- with(seedset, SQRT_SEEDWEIGHT)
modelsqrt2<-lme(SQRT_SEEDWEIGHT~Treatment, random=~1|clone, seedset, method= "REML")
summary(modelsqrt2)
plot(modelsqrt2, main="Weight SqRt.Residuals")
qqnorm(resid(modelsqrt2), main=" weightSqRtQQ")
shapiro.test(modelsqrt2$residuals)
table(seedset$seed_set)

##Visuals
help("boxplot")
par(mfrow=c(1,1))
boxplot(seed_set~Treatment, main= "Seed Set", ylab="Seed Set(seeds/flower)", xlab="Treatment")
boxplot(ave_seed_weight~Treatment, main="Seed Weight", ylab="Average Seed Weight (mg)", xlab="Treatment")
## model prediction
predictedscores<-predict(model3.1fit,re.form=NA, type="response")
table(predictedscores)
confint(model3.1fit)

