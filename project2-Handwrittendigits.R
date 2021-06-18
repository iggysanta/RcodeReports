# Prep libraries
library(rgl)
library(rattle)
library(corrplot)
library(tidyverse)
library(TeachingDemos)
library(Rtsne)
# BRING IN THE DATA
train <- read.table(file=
                      "http://archive.ics.uci.edu/ml/machine-learning-databases/optdigits/optdigits.tra",
                    sep=",", header = FALSE, na.strings = c("NA", "", " "),
                    col.names = c(paste("x", 1:64, sep=""), "digit"))
test <- read.table(file=
                     "http://archive.ics.uci.edu/ml/machine-learning-databases/optdigits/optdigits.tes",
                   sep=",", header = FALSE, na.strings = c("NA", "", " "),
                   col.names = c(paste("x", 1:64, sep=""), "digit"))
dim(train); dim(test)

dat <- rbind(train, test); 
dim(dat)

## PREPARE COLORS FOR PLOTTING 
Labels <- as.character(dat$digit)
colors <- rainbow(length(unique(Labels)))
names(colors) <- unique(Labels)

#library(rattle)
rattle()
dat$x1 <- NULL
dat$x40 <- NULL

COR <- cor(dat, use = "everything", method = "pearson")
round(COR, 2)

#library(corrplot)
corrplot(COR, method="ellipse")
#corrplot(COR, method="number") too many points to have the numbers in and be clear
#corrplot.mixed(COR, lower="number", upper="ellipse") too messy here

##############
dat0.scaled <- scale(dat0)

dat0 <- as.matrix(dat[order(dat$digit),])
dat0 <- dat0[,-63]
help(heatmap)
#heatmap(dat0, Colv = NA)

#heatmap(dat0,col = cm.colors(50), Colv = NA, labRow = FALSE, margins = c(4,4),
       # xlab = "Image Variables", ylab = "Samples",
       # main = "Heatmap of Handwritten Digit Data")

heatmap(dat0, col = cm.colors(256), scale = "column", Colv=NA,
        labRow=FALSE, margins = c(4,4),
        xlab = "Image Variables", ylab = "Samples",
        main = "Heatmap of Handwritten Digit Data")

############
#library(tidyverse)
#library(TeachingDemos)
dat %>%
  aggregate(by=list(dat$digit), FUN=mean) %>%
  select(20:35) %>% # SELECT 15 COLUMNS TO VIEW
  faces(nrow=2, ncol=5, labels=0:9)
############

pca.res <- prcomp(loans0, center = TRUE, scale. = TRUE, retx = TRUE); pca.res
new <- dat[1:10, ]; new
PC.new <- as.data.frame(predict(pca.res, newdata=new)); PC.new
PC.directions <- pca.res$rotation; PC.directions
a1.a2 <- pca.res$rotation[,1:2]; head(a1.a2)
names(pca.res)
pca.res$x[,1:3]

plot(pca.res$x[,1:2], pch= "", main="PC.1 and PC.2 for 'Dat0'", col = dat$digit)
text(pca.res$x[,1:2], labels=Labels, col=colors[Labels])
abline(v=0, lty=2)
abline(h=0, lty=2)



windows()
biplot(pca.res, scale=0, pc.biplot=FALSE, col=c("blue", "red"))
abline(v=0, col="grey35")
abline(h=0, col="grey35")
points(pca.res$rotation[,1], pca.res$rotation[,2], pch=18, cex=1, col="gray30")

plot(pca.res)
screeplot(pca.res); 
screeplot(pca.res, type="lines")

sd.pc <- pca.res$sdev 
var.pc <- sd.pc^2
prop.pc <- var.pc/sum(var.pc)

plot(prop.pc, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", type = "b")
plot(cumsum(prop.pc), xlab = "Principal Component", col="blue",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b", pch=19)

postscript(file="fig-PCAscree.eps", horizontal=TRUE)
windows() #add this to properly see
par(mar=c(4, 4, 4, 4))
bar <- barplot(var.pc, ylab="Variance Explained", col="skyblue", 
               xlab="Princinpal Components", col.axis="blue", col.lab="blue")
mtext(1:length(var.pc),side=1, line=1,at=bar,col="black")
par(new=T)
plot(bar, cumsum(prop.pc),axes=F, xlab="", ylab="", col="orange", type="b", 
     col.lab="orange")
axis(4,col="orange", col.ticks="orange", col.axis="orange")
abline(h=.90, lty=2, col="red", lwd=.8)
mtext("Cumulative Proportion of Variance Explained",side=4,line=3,col="orange")
title(main = 'Pareto Chart from PCA')# run to here the next closes the graph

apply(pca.res$x, 2, var)
######################
# Part 3 MDS
pc <- prcomp(loans0)
comp <- data.frame(pc$x[,1:4])
plot(comp, pch=16, col=colors[Labels]) # PAIRS PLOT
plot(comp[, 3:4], main = "MDS Plot of First 2 PC", pch=Labels, col=colors[Labels])

#####################

# PCA - CLASSICAL MDS
pc <- prcomp(loans[, -1])
comp <- data.frame(pc$x[,1:4])
plot(comp, pch=16, col=colors[Labels]) # PAIRS PLOT
plot(comp[, 1:2], main = "MDS Plot of First 2 PC for 'Dat'", pch=Labels, col=colors[Labels]) # FIRST TWO PCs

#library(rgl) # 3D PLOT reveals some of the clumping of numbers
plot3d(comp$PC1, comp$PC2, comp$PC3, col=colors[Labels])

## USING Rtsne
fit.tsne <- Rtsne(dat[,-63], dims = 2, perplexity=50, verbose=TRUE, max_iter=500)
plot(fit.tsne$Y, t='n', main="tSNE View of handwritten UCI Data",
     xlab=expression(z[1]), ylab=expression(z[2]))
text(fit.tsne$Y, labels=Labels, col=colors[Labels])
