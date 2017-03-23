rm(list=ls())
library(arm)
library(nlme)
library(dplyr)
library(ggplot2)
#library(plotrix)

setwd("~/Documents/git/carpen/NPJ work")
seed<-read.csv(file="Sedge Seed Data - Sheet1 (1).csv", h=TRUE)

#plotting data
names(seed)
#seed<-select_(seed,"Treatment","Clone","Seed_set","Ave_seed_weight")
seed$Seed_set<-as.numeric(as.character(seed$Seed_set)) ###this command is changing your values
seed<-na.omit(seed)
sapply(seed, class)

ggplot(seed, aes(x=Seed_set))+geom_histogram()
ggplot(seed, aes(x=Ave_seed_weight))+geom_histogram()

t.test(Seed_set~Treatment, data=seed)
t.test(Ave_seed_weight~Treatment, data=seed)

##model
weight<-lmer(Ave_seed_weight~Treatment+(1|Clone), data=seed )
yield<-glmer(Seed_set~Treatment+ (1|Clone),family = poisson(link="log"),nAGQ = 1, data=seed)

display(yield)
summary(yield)
coef(yield)
summary(weight)
display(weight)
###post hoc
plot(yield)
res<-residuals(yield)
qqnorm(resid(yield))
hist(res, breaks="FD", xlab="Residuals", 
     main="Histogram of residuals", ylim=c(0,50))
x <- -50:50


###model predictions
predictedscores<-predict(yield,re.form=NA, type="response")
table(predictedscores)
confint(yield)

predictedscores2<-predict(weight,re.form=NA, type="response")
table(predictedscores2)

###visualizations
c<-ggplot(seed, aes(x=Treatment, y=Seed_set))+labs( x = "Treatment", y = "Average number of seeds/inflorenscence") +stat_summary()
c

c2<-ggplot(seed, aes(x=Treatment, y=Ave_seed_weight))+labs( x = "Treatment", y = "Average seed weight (mg)") +stat_summary()
c2

c
c2

#why did I have different results before
seedset<-read.csv(file="seed_set_indv.csv", h=T)
t.test(seed_set~Treatment, data=seedset)
mean(seed$Seed_set)
mean(seedset$seed_set)
4.6/2.8
#done






#over dispersion## where did i find this
n<-175
k<-12
yhat <- predict (yield, type="response")
z <- (seedset$seed_set-yhat)/sqrt(yhat)
cat ("overdispersion ratio is ", sum(z^2)/(n-k), "\n")
cat ("p-value of overdispersion test is ", pchisq (sum(z^2), n-k), "\n")
## Ooverdispersion ratio is  2.132154 

library(AER) 
dispersiontest(yield,trafo=1) ## this doesnt work for glmer

###correcting se for over dispersion
sqrt(2.132154)
##multiply as standard regession errors by  1.46019 
#for treatment
0.08944*1.46019
##se=0.1305994
predict(yield$reatment)
citation("lme4")
###checking standard errors
out<-filter(seed,Treatment=="outcrossed")
inn<-filter(seed,Treatment=="selfed")
std.error(inn$Seed_set)
std.error(out$Seed_set)
