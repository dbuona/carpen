#This add culm_ID and flower_id as possible fixed effects
setindv=read.csv(file="seed_set_indv.csv", h=T)
attach(setindv)
library(nlme)
#Fit a model with "loaded Mean Structure
model3.1fit<-lme(seed_set~Treatment+culm_ID+flower_id, random=~1|clone, setindv, method= "REML")
summary(model3.1fit)
anova(model3.1fit)
random.effects(model3.1fit)
#structure for Random Effect
model3.1Afit<-gls(seed_set~Treatment+culm_ID+flower_id+Treatment*culm_ID+Treatment*flower_id, data=setindv)
anova(model3.1fit,model3.1Afit)
#to get te correct pvalue for model 3.1 must divide by 2 (page. 80)

# so pvalue is 0.027 and therfore we must keep random effect in all subsequent models
#Model 3.2A seperate residual variance for each treatment
model3.2Afit<-lme(seed_set~Treatment, random=~1|clone,seedset, method="REML", weights=varIdent(form=~1|Treatment))
summary(model3.2Afit)
#test hypothesis 3.2
anova(model3.1fit,model3.2Afit)
# because p-value is not significant, should use homogeneous varience model