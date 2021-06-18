library(mice)
library(MASS)
library(glmulti)
library(ncvreg)
library(boot)
library(caret)
library(cvAUC)
library(verification)
library(finalfit)
library(dplyr)
#?mice
setwd("F:/RworkFiles")
hcv <- read.csv("hcvdat0.csv", header = TRUE)

#--------------------------------------------------------------
hcv$Category <- ifelse(hcv$Category == "0=Blood Donor",0, ifelse(hcv$Category == "0s=suspect Blood Donor", 0, 1))
hcv$Sex <- as.factor(hcv$Sex)
hcv <- subset(hcv, select = -X)
#---------------------------------------------------------------
#c
615-540
75/615
indxTrain <-createDataPartition(y = imputedhcv$Category, p = .8, list = FALSE)
#about 12.2% of the population has Hep C
# it could be an issue as there is such a small portion but 
#-------------------------------------------------------------
sum(is.na(hcv))
#there is some missing data total of 31 points
#although alp is missing only among those classified as 1

# 1 D
imputedhcv <- mice(hcv)
imputedhcv <- complete(imputedhcv)
imputedhcv$Sex <- as.factor(imputedhcv$Sex)
sum(is.na(imputedhcv))

# moved to impute data via the mice package I could have gone with deletion as 
# a good portion of the target was missing 

#-----------------------------------------------------------------
#2 A
#We have changed Category  to 0 or 1. Age is whole integers 
# sex is set to factor although we can change it to binary as well.
# the other 10 variables are continuous.
#***************************************
#*
# 2B
# We move to fit the full model as there will be a fit.full this will be called fit.all
formula0 <- Category ~ Age + Sex + ALB + ALP + ALT + AST + BIL + CHE + CHOL + CREA + GGT + PROT
fit.all <- glm(formula0, family = binomial, data = imputedhcv)
summary(fit.all);BIC(fit.all, fit.full)
#2C
# Looking out the output of the all model i will choose sex, ALP, ALT, AST, BIL, CHOL, CREA,GGT,PROT,and as it is < the .2 alpha value we are using we will include ALB as well. In the end we only dropped the AGE and CHE variables as they did not meet the criteria 
formula1 <-Category ~ Sex + ALB + ALP + ALT + AST + BIL + CHOL + CREA + GGT + PROT # this will be our fit.full
#_______________________________________________________________

#3 A
fit.full <- glm(formula1, family = binomial, data = imputedhcv)

#________________________________________________________________

#3 B
#?stepAIC
fit.step <- stepAIC(fit.full, direction = "both",  k=log(n), trace = FALSE)
#warnings()
fit.step$anova
summary(fit.step) 
BIC(fit.step)

#____________________________________________________________

#3 C

fitting <- glmulti(formula1, data = imputedhcv, fitfunc = glm, family=binomial, intercept = TRUE, 
                   crit = bic, level = 1, method="g", confsetsize=1)  
fit.BSS <- attributes(fitting)$objects[[1]]
fit.BSS$coef 
summary(fit.BSS) 
BIC(fit.BSS)
#_________________________________________________________________

y <- imputedhcv$Category
X <- model.matrix(object=~ Sex + ALB + ALP + ALT + AST + BIL + CHOL + CREA + GGT + PROT,
                  data=imputedhcv)
cvfit.SCAD <- cv.ncvreg(X=X,y=y, nfolds=5, family="binomial", penalty="SCAD", 
                        lambda.min=.001, nlambda=500, eps=.01, max.iter=5000) 


plot(cvfit.SCAD)
result.SCAD <- cvfit.SCAD$fit
beta.hat <- as.vector(result.SCAD$beta[-1, cvfit.SCAD$min])
cutoff <- 0
terms <- colnames(X)[abs(beta.hat) > cutoff]; terms
formula.SCAD <- as.formula(paste(c("Category ~ 1", terms), collapse=" + "))
fit.pen <- glm(formula.SCAD, data = imputedhcv, family="binomial")
summary(fit.pen)
BIC(fit.pen)
###############
# Written report about the best fitting models
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
BIC(fit.pen,fit.BSS,fit.step)
# 4 
par(mfrow = c(1,2))
# Creating ROC curve graphs
# 4 A) Jacknife preditions

# SCAD jackknife
n <- NROW(imputedhcv)
p.jk <- rep(0, n)
for (i in 1:n){
  # print(i)
  fit.i <- glm(formula(fit.pen), data=imputedhcv[-i,], family = "binomial")
  p.jk[i] <- predict(fit.i, newdata=imputedhcv[i,], type="response")
}

confusionMatrix(factor(sign(p.jk >= 0.5)), factor(y))

tc <- trainControl("cv", number=10, savePred=TRUE)    
fit.cv <- train(factor(Category) ~ ALP + AST + BIL + CHOL + CREA + GGT, data=imputedhcv, method="glm", 
                trControl=tc, family=binomial(link = "logit"))    
fit.cv
names(fit.cv)
fit.cv$finalModel; fit.cv$"results"


# SCAD

y <- imputedhcv$Category; yhat <- p.jk
a.ROC <- roc.area(obs=y, pred=yhat)$A
print(a.ROC) # AUC
# .9497037
AUC <- ci.cvAUC(predictions=yhat, labels=y, folds=1:n, confidence=0.95); AUC 
auc.ci <- round(AUC$ci, digits=3)
mod.glm <- verify(obs=y, pred=yhat)
roc.plot(mod.glm, plot.thres = NULL)
text(x=0.7, y=0.2, paste("Area under ROC =", round(AUC$cvAUC, digits=3), 
                         "with 95% CI (", auc.ci[1], ",", auc.ci[2], ").",
                         sep=" "), col="blue", cex=1.2)







#__________________________________________________________________________
#4 B)



# As the BSS and the step models provided the same model i will only run the jackknife on one
n <- NROW(imputedhcv)
p.jk <- rep(0, n)
for (i in 1:n){
  # print(i)
  fit.i <- glm(formula(fit.BSS), data=imputedhcv[-i,], family = "binomial")
  p.jk[i] <- predict(fit.i, newdata=imputedhcv[i,], type="response")
}

confusionMatrix(factor(sign(p.jk >= 0.5)), factor(y))

tc <- trainControl("cv", number=10, savePred=TRUE)    # V=10
fit.cv <- train(factor(Category) ~ Sex + ALP + ALT + AST + BIL + CREA + GGT + PROT, data=imputedhcv, method="glm", 
                trControl=tc, family=binomial(link = "logit"))    
fit.cv
names(fit.cv)
fit.cv$finalModel; fit.cv$"results"



# ROC for BSS
# A DETAILED LOOK AT HOW THE ROC CURVE IS MADE 
y <- imputedhcv$Category; yhat <- p.jk
# ----------------------------
a.ROC <- roc.area(obs=y, pred=yhat)$A
print(a.ROC) # AUC
# .9711852
AUC <- ci.cvAUC(predictions=yhat, labels=y, folds=1:n, confidence=0.95); AUC 
auc.ci <- round(AUC$ci, digits=3)
mod.glm <- verify(obs=y, pred=yhat)
roc.plot(mod.glm, plot.thres = NULL)
text(x=0.7, y=0.2, paste("Area under ROC =", round(AUC$cvAUC, digits=3), 
                         "with 95% CI (", auc.ci[1], ",", auc.ci[2], ").",
                         sep=" "), col="blue", cex=1.2)
#-----------------------------------------------------------------------------
# 5
# this will produce CI for all 
sum1 <- summary(fit.final, dispersion=1); sum1
sum1$cov.unscaled
ci <- confint(fit.final, parm=c("Sexm", "ALP","ALT","BIL","CREA","GGT","PROT"), level = 0.95)
ci
# This will produce the odds ratio
ci <- confint(fit.final, level = 0.95); 
exp(ci*10) 

#ci <- confint(fit.final, parm=c("Sexm"), level = 0.95)
#exp(ci*1) # this will show the effect of being male

#exp(ci*1)








#

