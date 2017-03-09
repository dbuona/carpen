rm(list=ls())
library("arm")
library("nlme")
seedset=read.csv(file="seedsetdata.csv", h=T)
attach(seedset)
###model
yield<-glmer(seed_set~Treatment+ (1|clone),family = poisson(link="log"),nAGQ = 1)
display(yield)
summary(yield)
##test for over dispersion
n<-175
k<-12
yhat <- predict (yield, type="response")
z <- (seed_set-yhat)/sqrt(yhat)
cat ("overdispersion ratio is ", sum(z^2)/(n-k), "\n")
cat ("p-value of overdispersion test is ", pchisq (sum(z^2), n-k), "\n")
## Ooverdispersion ratio is  2.132154 
install.packages("AER")
library(AER) 
dispersiontest(yield,trafo=1) ## this doesnt work for glmer

###correcting se for over dispersion
sqrt(2.132154)
 ##multiply as standard regession errors by  1.46019 
 #for treatment
 0.08944*1.46019
 ##se=0.1305994
 
 ### or try pql from over dispersion
yieldPQL<- glmmPQL(seed_set ~Treatment, ~1|clone, family = poisson(link = "log"), verbose = FALSE)
display(yieldPQL)##no display argument for PQL
summary(yieldPQL)
### not sure if this model is acutally better based on small amount of over dispersion
##The statistics helper told me to just use the "yield model"
predictedscores=predict(yield,re.form=NA, type="response")
table(predictedscores)
predictedscores2=predict(yieldPQL,level = 0, re.form=, type="response")
table(predictedscores2)
confint(yield)
91+84

