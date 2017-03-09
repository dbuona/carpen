rm(list=ls())
library("arm")
library("nlme")
library(ggplot2)

setwd("~/Documents/git/carpen/NPJ work")
seedset=read.csv(file="seed_set_indv.csv", h=T)
#plotting data
c <- ggplot(seedset, aes(x=seed_set))+geom_histogram()
c
t.test(seed_set~Treatment, data=seedset)

##model
yield<-glmer(seed_set~Treatment+ (1|clone),family = poisson(link="log"),nAGQ = 1, data=seedset)

display(yield)
summary(yield)
coef(yield)

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

###visualizations
ggplot(yield, aes(x=Treatment, y=seed_set)) +stat_summary()
ggplot(yield, aes(x=Treatment, y=seed_set))+geom_boxplot()




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
