#setwd("F:/RworkFiles")

#--------------------
# Read in data
#--------------------
baseball <- read.table(file = "http://www.amstat.org/publications/jse/datasets/baseball.dat.txt", header = F, col.names=c("salary", "batting.avg", "OBP", "runs", "hits", "doubles", "triples", "homeruns", "RBI", "walks", "strike.outs", "stolen.bases", "errors", "free.agency.elig", "free.agent.91", "arb.elig", "arb.91", "name"))
#warnings()
head(baseball)

#--------------------------
# prep libraries
#--------------------------
library(rattle)
library(glmulti) 
library(leaps)
library(rJava)
library(bestglm)
library(car)
library(carData)
library(snpar)
library(ggplot2)
library(MESS)
library(glmnet)
#rattle()
#install.packages("RGtk2")
#install.packages("carData")
#install.packages("car")
#----------------------------------
#read in new to have it ready
#---------------------------------
?read.csv
new <- read.csv(file = "bb92-test.csv", header = TRUE)
new <- data.frame(new)
#______________________________________________________

par(mfrow=c(2,2),mar=c(4, 4, 4, 4))
hist(baseball$salary, xlab="salary", main="Histogram of Salary")
qqnorm(baseball$salary, main="Q-Q Plot of Salary")
qqline(baseball$salary)
hist(log(baseball$salary), xlab="log-alary", main="Histogram of Log(Salary)")
qqnorm(log(baseball$salary), main="Q-Q Plot of Log(Salary)")
qqline(log(baseball$salary))
#dev.off()
sum(is.na(baseball))

#------------------------------------------------
# Turned salary in to natural log
#------------------------------------------------
baseball$salary <- log(baseball$salary)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#check BIC of all at once
BIC(fit.fulltrain, fit.BSS, fit.backward, fit.step, fit.subset, fit.ALASSO)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#_______________________________________________
# this creates my training and test sets
#_______________________________________________
# remove these as they are a 1 or 0 dummy results show almost the same as if i had left them
##dat$cat1 <- cut(dat$free.agency.elig, breaks = 2, labels = LETTERS[1:2], ordered_result = TRUE)
##dat$cat2 <- cut(dat$free.agent.91, breaks = 2, labels = LETTERS[1:2], ordered_result = TRUE)
##dat$cat3 <- cut(dat$arb.elig, breaks = 2, labels = LETTERS[1:2], ordered_result = TRUE)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# NOT USED
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
dat <- baseball
dt <- sort(sample(nrow(dat),nrow(dat)*.65))
train <- dat[dt,]
test <- dat[-dt,]
str(train)
#==========================================================
# Full set of estimators
#==========================================================
fit.fulltrain <- lm(salary ~ batting.avg + OBP + runs + hits + doubles + triples + homeruns + RBI + walks + strike.outs + stolen.bases + errors + free.agency.elig + free.agent.91 + arb.elig + arb.91, data = train)
BIC(fit.fulltrain)

#+++++++++++++++++++++++++++++++++++++++++++
# Best Subset Selection to find model
#+++++++++++++++++++++++++++++++++++++++++++
formula1 <- salary ~ batting.avg + OBP + runs + hits + doubles + triples + homeruns + RBI + walks + strike.outs + stolen.bases + errors + free.agency.elig + free.agent.91 + arb.elig + arb.91 -1

y <- train[, all.vars(formula1)[1]]
X <- as.data.frame(model.matrix(as.formula(formula1),train))

dat.tmp <- as.data.frame(cbind(X, y))	
result.bestBIC <- bestglm(Xy=dat.tmp, 
                          IC="BIC", intercept=TRUE, TopModels=5);   
# Setting intercept=TRUE means the intercept term is always included
names(result.bestBIC)
result.bestBIC$"qTable"
result.bestBIC$BestModels  
result.bestBIC$BestModel

fit.subset <- result.bestBIC$BestModel
summary(fit.subset); anova(fit.subset)
BIC(fit.subset)
#===========================================
# Best Subset selection via library(glmulti)
#===========================================

fit.glm <- glmulti(salary ~ batting.avg + OBP + runs + hits + doubles + triples + homeruns + RBI + walks + strike.outs + stolen.bases + errors + free.agency.elig + free.agent.91 + arb.elig + arb.91, data = train, fitfunc = lm, intercept = TRUE, crit = bic, level = 1, method="g", confsetsize=1)
fit.BSS <- attributes(fit.glm)$object[[1]]
fit.BSS$coef
summary(fit.BSS); anova(fit.BSS)
BIC(fit.BSS) #moved up top will add third 

#-------------------
# Adaptive lasso 
#--------------------


set.seed(123)
# SCALE DATA
formula0 <- salary ~ batting.avg + OBP + runs + hits + doubles + triples + homeruns + RBI + walks + strike.outs + stolen.bases + errors + free.agency.elig + free.agent.91 + arb.elig + arb.91 -1		# NO QUOTATION MARKS & NO INTERCEPT	
y <- train[, all.vars(formula0)[1]]
X <- model.matrix(as.formula(formula0),train)
dat1 <- as.data.frame(cbind(X, y))	

wt <- adaptive.weights(x=X, y=y, weight.method="univariate")
cv.fit <- cv.glmnet(x=as.matrix(X), y=y, family="gaussian", alpha=1, nlambda=100,
                    penalty.factor=as.numeric(wt$weights), standardize=TRUE)
plot(cv.fit)
# beta.hat <- coef(cv.fit, s="lambda.min")	# 0SE
beta.hat <- coef(cv.fit, s="lambda.1se")  	# 1SE FOR BETTER SELECTION

# AGAIN, LET'S FIT OLS MODEL WITH ALASSO SELECTED VARIABLES
# ALTHOUGH THIS IS UNNECESSARY SINCE ALASSO ENJOYS THE ORACLE PROPERTY
cutoff <- 0
terms <- row.names(beta.hat)[abs(as.vector(beta.hat)) > cutoff]; 
terms <- terms[which(terms != "(Intercept)")]  # REMOVE intercept
formula.ALASSO <- as.formula(paste(c("salary ~ ", terms), collapse=" + "))
fit.ALASSO <- lm(formula.ALASSO, data = dat)
summary(fit.ALASSO)
BIC(fit.ALASSO)

#=============================================
# backward deletion and stepwise to find model
#=============================================

# BACKWARD DELETION
fit.backward <- step(fit.fulltrain, direction="backward", k=log(NROW(train)), trace=TRUE)
names(fit.backward); 
fit.backward$anova; fit.backward$coefficients
summary(fit.backward)
BIC(fit.backward)

fit.nulltrain <- lm(salary ~ 1, data=train)
fit.step <- step(fit.nulltrain, scope=list(lower=fit.nulltrain, upper=fit.fulltrain), direction="both", k=log(NROW(train)))
fit.step$anova; fit.step$coefficients
summary(fit.step)
BIC(fit.step)
#
fit.BSS$coef; fit.step$coefficients; fit.ALASSO$coefficients
#
#_________________________________________________________
# Best Model
best.model <- salary ~ runs + RBI + free.agency.elig + arb.elig
BIC(fit.backward, fit.step, fit.subset)
#---------------------------------------------------------

?lm
BSS.test <- lm(salary ~ hits + RBI + strike.outs + free.agency.elig + arb.elig , data = test)
step.test <- lm(salary ~ hits + RBI + strike.outs + free.agency.elig + arb.elig, data = test)
lasso.test <- lm(salary ~ batting.avg + OBP + doubles + homeruns + free.agency.elig + free.agent.91 + arb.elig, data = test)

BIC(BSS.test, step.test, lasso.test)
SSPE1 <- sum(step.test$residuals^2)
SSPE2 <- sum(BSS.test$residuals^2)
SSPE3 <- sum(lasso.test$residuals^2)
SSPE1;SSPE2;SSPE3

# the hits model which wasnt as good as the runs model on the training data
#so moving to use the hits model on the final data


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Applying chosen model to the full data set
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

fit.final <- lm(salary ~ hits + RBI + strike.outs + free.agency.elig + arb.elig, data = dat)
summary(fit.final)
plot(dat$salary, fit.final$fitted.values)
abline(a = 0, b = 1, col = "red")

#=======================
#Diagnostics
#=======================
jackk <- rstudent(fit.final)

par(mfrow=c(1,2),mar=c(8,4,8,4)) 
hist(jackk, xlab="Jackknife Residual", main="(a) Histogram") 
qqPlot(fit.final, pch=19, cex=.8, col="red", main="(b) Q-Q Plot") 
shapiro.test(jackk) 
#data:  jackk
#W = 0.95423, p-value = 9.586e-09
# so we assume that the data is non-normal
#//////////////////////////////////////////////
ncvTest(fit.final)
par(mfrow=c(1,1),mar=c(4, 4, 4, 4)) 
spreadLevelPlot(fit.final, pch=20, cex=0.5, col="green4", 
                main="Fit.Final Model Baseball$Salary: Heteroscedasticity")
#Non-constant Variance Score Test 
#Variance formula: ~ fitted.values 
#Chisquare = 0.9404241, Df = 1, p = 0.33217
## with a P-Value > .05 we maintain the assumption of Homoscedasticity 
#/////////////////////////////////////////////////////
durbinWatsonTest(fit.final)
runs.test(jackk, exact=FALSE, alternative="two.sided")
#we fail to reject that this model is not commiting auto correlation errors
#/////////////////////////////////////////////////////
crPlots(fit.final, main="Partial Residual Plots")
leveragePlots(fit.final, main="Partial Regression (Leverage) Plots") 
ceresPlots(fit.final) 
#warnings()
#this shows that all variables maintain liniarity although free agency shows signs of non-liniarity
#///////////////////////////////////////////////////////////////////////
#library(gvlma)
#gvlma(fit.final)
#////////////////////////////////////////////////////////////////
n <- NROW(baseball)
p <- length(coef(fit.final))-1
infl <- influence.measures(fit.final); 
infl.mat <- as.data.frame(infl$infmat)
infl.sum <- summary(influence.measures(fit.final)); 
infl.sum 

write.csv(infl.sum, file="Infleunce-Mat.csv", row.names=TRUE)
cook.d <- infl.mat$cook.d; cook.d
outlierTest(fit.final)
cutoff <- 4/(n-p-2)
plot(fit.final, which=4, cook.levels=cutoff, col="gray65", lwd=1.5)
points(1:n, cook.d, pch=1, cex=1, col="blue")   
baseball[cook.d > 0.05, ]
influencePlot(fit.final, id=list(method="identify"), 
              col="blue", 
              main="Influence Plot", 
              sub="Circle size is proportial to Cook's d")
#////////////////////////////////////////////////////////////////////
?lm
fit<- lm(baseball$salary ~ baseball$hits + baseball$RBI + baseball$strike.outs + baseball$free.agency.elig + baseball$arb.elig, x=TRUE);
kappa(fit$x)

kappa(lm(baseball$salary ~ baseball$hits + baseball$RBI + baseball$strike.outs + baseball$free.agency.elig + baseball$arb.elig -1, x = TRUE)$x, data = fit.final)
vif(fit.final)
#----------------------------------------------------------------
# model deployment
#----------------------------------------------------------------
deployment <- predict(fit.final, newdata = new, se.fit = TRUE, interval = "prediction", level = 0.95)
deployment
pred <- predict(fit.final, newdata = new, interval="prediction");
dat.plot <- data.frame(player=1:NROW(new), exp(deployment$fit)); names(dat.plot)
ggplot(dat.plot, aes(x=player, y=fit)) +
  geom_errorbar(aes(ymin=lwr, ymax=upr)) + geom_point()







#
