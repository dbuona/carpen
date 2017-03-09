### my thesis is a mess. Here is a summary of the the apparently 2 different scripts I used.
### GLM code has the predicted values and 
##here is the best model for seed set I think
GHQ <- glmer(seed_set ~Treatment + (1 |clone), data = seedset, family = poisson(link = "log"), nAGQ = 2)
summary(GHQ)
##for seed weightm neither clone nor 
model4.1fit<-lme(ave_seed_weight~Treatment, random=~1|clone, seedset, method= "REML")
summary(model4.1fit)
anova(model4.1fit)
random.effects(model4.1fit)
#structure for Random Effect
model4.1Afit<-gls(ave_seed_weight~Treatment, data=seedset)
anova(model4.1fit,model4.1Afit)
.8015/2
linearweight<-lm(ave_seed_weight~Treatment)
summary(linearweight)
### here are the plots

### predicted values for seed set:
predictedscores=predict(GHQ,re.form=NA, type="response")
table(predictedscores)
confint(GHQ,seed_set,level = 0.95)


