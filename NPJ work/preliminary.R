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
##then I tried a bunch of transformations, but nothing work. Then a statistics person told me to use glmer
##visuals
par(mfrow=c(1,1))
boxplot(seed_set~Treatment, main= "Seed Set", ylab="Seed Set(seeds/flower)", xlab="Treatment")
boxplot(ave_seed_weight~Treatment, main="Seed Weight", ylab="Average Seed Weight (mg)", xlab="Treatment")
