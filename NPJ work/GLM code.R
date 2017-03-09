#THis is using GLM based on http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
setwd("~/Desktop/SNRE/Thesis_workingdirectory")
rm(list=ls()) 
install.packages("car")
install.packages("MASS")
library(car)
library(MASS)
seed=read.csv(file="seedsetdata.csv", h=T)
qqp(seed$seed_set, "norm")
qqp(seed$seed_set, "lnorm") #this distrubution is best
nbinom <- fitdistr(seed$seed_set, "Negative Binomial")
qqp(seed$seed_set, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(seed$seed_set, "Poisson")
qqp(seed$seed_set, "pois", poisson$estimate)
gamma <- fitdistr(seed$seed_set, "gamma")
qqp(seed$seed_set, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
#lognormal seems to be the best fit
#PQL
PQL <- glmmPQL(seed_set ~Treatment, ~1|clone, family = gaussian(link = "log"), data = seed, verbose = FALSE)
summary(PQL)
#response variable mean is <5 so...Test Gauss-Hermite quadratur vs. Laplace method
install.packages("mlmRev")
library(mlmRev)
#############
GHQ <- glmer(seed_set ~Treatment + (1 |clone), data = seed, family = poisson(link = "log"), nAGQ = 2)
summary(GHQ)
predictedscores=predict(GHQ,re.form=NA, type="response")
table(predictedscores)
confint(GHQ)
library(lme4)
library(car)
anova(GHQ)

cloneset<-lm(seed_set~clone, data=seedset)
summary(cloneset)
help(confint)
library(lme4)




install.packages("visreg")
library(visreg)
visreg(GHQ,scale="response")
predictedscoreswithse=predict(GHQ,re.form=NA, type="response",se=TRUE)



