library(Hmisc)
library(tidyverse)
library(VIM)
library(dplyr)
library(mice)
library(cluster)
library(Rtsne)
library(stats)
library(mclust)
library(clusteval)
library(fpc) #error in loading...
getwd()
setwd("F:/RworkFiles")
#=================
# Loading in the data file
#=================
help("read.csv")
loans <- read.csv(file = "HMEQ.csv", header = TRUE, na.string=c("", " ", "NA", "?"))
dim(loans); head(loans)
dat <- read.csv(file = "HMEQ.csv", header = TRUE, na.string=c("", " ", "NA", "?"))
#----------------------

??Hmisc
#---------------------
## Missing percentages
#---------------------
anyNA(loans)
miss.info <- function(dat, filename=NULL){
  vnames <- colnames(dat); vnames
  n <- nrow(dat)
  out <- NULL
  for (j in 1: ncol(dat)){
    vname <- colnames(dat)[j]
    x <- as.vector(dat[,j])
    n1 <- sum(is.na(x), na.rm=T)
    n2 <- sum(x=="NA", na.rm=T)
    n3 <- sum(x=="", na.rm=T)
    nmiss <- n1 + n2 + n3
    ncomplete <- n-nmiss
    out <- rbind(out, c(col.number=j, vname=vname, 
                        mode=mode(x), n.levels=length(unique(x)), 
                        ncomplete=ncomplete, miss.perc=nmiss/n))
  }
  out <- as.data.frame(out)
  row.names(out) <- NULL 
  if (!is.null(filename)) write.csv(out, file = filename, row.names=F)
  return(out)
}
miss.info(loans)

aggr(loans, col = c("blue", "orange", "red"), numbers = TRUE, sortVars = TRUE, borders = "gray60", labels = names(loans), cex.axis = .5, gap = 2, ylab= c("missing%", "Pattern"))

#==========================
# Replacing missing values in Reason and Job
#==========================

loans <- loans %>%
  mutate(REASON = ifelse(is.na(REASON), "UNKNOWN", REASON))
loans <- loans %>%
  mutate(JOB = ifelse(is.na(JOB), "UNKNOWN", JOB))
head(loans)
# for the dat as well
dat <- dat %>%
  mutate(REASON = ifelse(is.na(REASON), "UNKNOWN", REASON))
dat <- dat %>%
  mutate(JOB = ifelse(is.na(JOB), "UNKNOWN", JOB))
head(dat)
#---------------------
## Missing percentages after the changing of REASON and JOB NAs
#---------------------
anyNA(loans)

miss.info(loans)

aggr(loans, col = c("blue", "orange", "red"), numbers = TRUE, sortVars = TRUE, borders = "gray60", labels = names(loans), cex.axis = .5, gap = 2, ylab= c("missing%", "Pattern"))
#--------------------------------
# Natural log for standarization
#--------------------------------
loans$VALUE <- log(loans$VALUE)
loans$LOAN <- log(loans$LOAN)
loans$MORTDUE <- log(loans$MORTDUE)
loans$YOJ <- log1p(loans$YOJ) #as there are ZEROs in the data this will control for it
loans$CLAGE <- log1p(loans$CLAGE)
#view(loans)
#----------------------------------
dat$LOAN <- log(dat$LOAN)
dat$MORTDUE <- log(dat$MORTDUE)
dat$YOJ <- log1p(dat$YOJ) #as there are ZEROs in the data this will control for it
dat$CLAGE <- log1p(dat$CLAGE)
#----------------------------------

loans.x <- loans %>% select(-BAD)
?mice
cols.cat <- c(4,5)
for (j in cols.cat) loans.x[, j] <- as.factor(loans.x[, j])

?md.pattern
par(mfrow=c(1,1))
md.pattern(loans.x)
fit.mice <- mice(loans.x, m=1, maxit = 10, method = 'pmm', seed = 123, diagnostics = FALSE, remove_collinear = FALSE)

fit.mice$loggedEvents
loans.imputed <- mice::complete(fit.mice, 1)

summarise(loans.imputed)
anyNA(loans.imputed)
colMeans(is.na(loans.imputed))
colMeans(is.na(loans))
#------------------------------------------
#Random Forest
dat <- as.data.frame(loans.imputed)
names(dat); dim(dat)
head(dat)

# CHECK THE VARIABLE TYPES
apply(dat, 2, FUN=class)
str(dat)


loans0 <- model.matrix(~.-1, data = loans.imputed)

#//////////////////////////////////////
# loans0 is matrix and dat is dataframe
#//////////////////////////////////////

?model.matrix

#view(loans.imputed)

#?daisy
#=======================
# Distance 
#=======================
#d <- dist(loans0, method = "euclidean")#Distance mat
D <- daisy(loans0, metric = 'gower', stand = TRUE)#disssimilarity
#-----------------------
# Up to here data is natural log, no missing data, and a distance mat is made
#-----------------------
#+++++++++++++++++++++++++++++
# Start of clustering analysis
#+++++++++++++++++++++++++++++

fit.ward<- hclust(D, method="ward.D2")
plot(fit.ward, hang = -1)

K.max <- 25
height <- tail(fit.ward$height, n=K.max)
n.cluster <- tail((nrow(loans0)-1):1, n=K.max)
plot(n.cluster, height,  type="b", pch=19, cex=.5, xlab="number of clusters", 
     ylab="height", col="blue", lwd=2)
abline(v=6, col = "gray", lty = 1)

K.star <- 6
plot(fit.ward, hang=-1) 
groups <- cutree(fit.ward, k=K.star) 
rect.hclust(fit.ward, k=K.star, border=2:(K.star+1))
table(groups, loans$BAD)

K <- 9
fit <- kmeans(loans0, K)
aggregate(loans0,by=list(fit$cluster),FUN=mean)
#dat1 <- data.frame(loans0, h.cluster = groups) adds the dummy variables
# EDA
clustering <- cutree(fit.ward, k=K.star)
n.cluster <- length(unique(clustering))
vnames <- names(loans.x); vnames
#head(dat)
cols.x <- 1:12
cols.cat.x <- c(4,5)
# par(mfrow=c(1, 1))
for (j in cols.x){
  if (is.element(j, cols.cat.x)) {
    print(table(clustering, dat[,j]))
  } else {
    # windows()
    boxplot(loans[,j]~clustering, xlab="cluster", ylab=vnames[j], col=1:n.cluster)
  }
}

#-----------------------------------
# SCREE PLOT for Determining number of clusters
wss <- (nrow(loans0))*sum(apply(loans0,2,var))
wss
K.max <- 25
for (K in 2:K.max) wss[K] <- sum(kmeans(loans0, centers=K)$withinss)
plot(1:K.max, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 
abline(v=7, col="blue", lty=2)
#-----------------------------------
#=======================================
K <- 7

# K-Means Cluster Analysis
fit <- kmeans(loans0, K, nstart=5, iter.max = 100) # K cluster solution
# get cluster means 

aggregate(loans0,by=list(fit$cluster),FUN=mean)
# append cluster assignment to each observation
dat1 <- data.frame(loans0, km.cluster=fit$cluster, true.cluster=loans$BAD) 


table(dat1$km.cluster, dat1$true.cluster)
# Compare two cluster memberships obtained by k-means and Hierarchical clustering
table(loans1$true.cluster, loans1$h.cluster)

#---------------------------------------------
?kmeans
cov(loans0); cor(loans0)


Labels <- as.character(fit$cluster)
Labels <- as.character(loans$BAD)
colors <- rainbow(length(unique(Labels)))
names(colors) <- unique(Labels)
loans.imputed$BAD <- loans$BAD


## USING Rtsne
?Rtsne
fit.tsne <- Rtsne(loans0, dims = 2, perplexity=250, verbose=TRUE, max_iter=500, theta = 0.1)
plot(fit.tsne$Y, t='n', main="tSNE View of LOAN data",
     xlab=expression(z[1]), ylab=expression(z[2]))
text(fit.tsne$Y, labels=Labels, col=colors[Labels])

#pc <- prcomp(loans0)
#comp <- data.frame(pc$x[,1:4])
#plot(comp, pch=16, col=colors[Labels]) # PAIRS PLOT
#plot(comp[, 2:3], main = "MDS Plot of First 2 PC for 'Dat'", pch=Labels, col=colors[Labels]) # FIRST TWO PCs

cluster_similarity(groups, dat1$km.cluster, similarity = "jaccard", method = "independence")
cluster_similarity(groups, dat1$km.cluster, similarity = "rand", method = "independence")
table(groups, dat1$km.cluster)
