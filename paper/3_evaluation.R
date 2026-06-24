# Packages
library(sfsmisc)

# Working directory
setwd("~/Nextcloud INRAE/RESOTEC/AI_Flickr")

# Load data
swt <- read.csv("outputs/trainswt_results.csv")
train <- read.csv("outputs/train/train_classification_report.csv")
train[1:5,] <- train[c(2,1,4,5,3),]
val <- read.csv("outputs/val/val_classification_report.csv")
val[1:5,] <- val[c(2,1,4,5,3),]
test <- read.csv("outputs/test/test_classification_report.csv")
test[1:5,] <- test[c(2,1,4,5,3),]
holdout <- read.csv("outputs/holdout/holdout_classification_report.csv")
holdout[1:5,] <- holdout[c(2,1,4,5,3),]
external <- read.csv("outputs/external/external_classification_report.csv")
external[1:5,] <- external[c(2,1,4,5,3),]

# Labels
lbl <- rev(train$labels[1:9])
nlbl <- length(lbl)

# Extract values
prec <- cbind(train$precision, val$precision, test$precision, 
              holdout$precision, external$precision)
recall <- cbind(train$recall, val$recall, test$recall, 
                holdout$recall, external$recall)
f1 <- cbind(train$f1.score, val$f1.score, test$f1.score, 
            holdout$f1.score, external$f1.score)

# Boxplots
box = list()
length(box) <- nlbl
for(k in 1:nlbl){
  box[[k]] <- swt$precision[swt$labels == lbl[k]]
}
boxa <- boxplot(box, plot=F, xaxt="n")
for(k in 1:length(box)){
  boxa$stats[1,k]=min(box[[k]])
  boxa$stats[5,k]=max(box[[k]])
}

box = list()
length(box) <- nlbl
for(k in 1:nlbl){
  box[[k]] <- swt$recall[swt$labels == lbl[k]]
}
boxb <- boxplot(box, plot=F, xaxt="n")
for(k in 1:length(box)){
  boxb$stats[1,k]=min(box[[k]])
  boxb$stats[5,k]=max(box[[k]])
}

box = list()
length(box) <- nlbl
for(k in 1:nlbl){
  box[[k]] <- swt$f1.score[swt$labels == lbl[k]]
}
boxc <- boxplot(box, plot=F, xaxt="n")
for(k in 1:length(box)){
  boxc$stats[1,k]=min(box[[k]])
  boxc$stats[5,k]=max(box[[k]])
}

# Figure 2
pdf("figures/Fig2.pdf",width=14.14, height=5.08, useDingbats=FALSE)
  
  colo <- "lightgrey"
  colopt <- c("#5F9DFB","#32CD32","#FFCC00","#C363C5","#F8766D")
  pchpt <- 21:25
  
  layout(matrix(c(1,2,3), 1, 3, byrow = TRUE),widths=c(4,2.9,2.9))
  
  #a
  par(mar=c(6,13,3,1))
  bxp(boxa, notch=FALSE, outline=FALSE,
      outcol=colo, boxcol=colo, whiskcol=colo, whisklty="solid", whisklwd=2,
      staplelwd=2, boxwex=0.5, staplecol=colo, medbg=colo, boxfill=colo, pch=16,
      ylim=c(0.3,1), 
      xlab="", ylab="", xaxt="n", yaxt="n", xaxs="i",
      main="", axes=FALSE, horiz=TRUE)
  axis(1, las=1, cex.axis=2, lwd=1.5, padj=0.25)
  axis(2, at=1:nlbl, labels=lbl, las=2, tick=FALSE, cex.axis=2, font=2, padj=0.5)
  mtext("Precision", 1, line=4.5, cex=1.75)
  title(main="(a)", cex.main=2.5, line=1)
  box(lwd=2)
  abline(v=0.8, lty=2, lwd=2, col=colo)
  
  par(new = TRUE)
  matplot(prec, 9:1, col=colopt, bg=colopt, pch=pchpt, cex=2,
          xlim=c(0.3,1), ylim=c(0.5,9.5),
          xlab="", ylab="", xaxt="n", yaxt="n",
          main="", axes=FALSE)
  
  legend("bottomleft", inset=c(0,-0.01), 
         legend=c("Train", "Validation", "Test", "Holdout", "External"),
         pch=pchpt, col=colopt, pt.bg=colopt,
         bty="n", cex=2, xpd=TRUE)
  
  #b
  par(mar=c(6,1,3,1))
  bxp(boxb, notch=FALSE, outline=FALSE,
      outcol=colo, boxcol=colo, whiskcol=colo, whisklty="solid", whisklwd=2,
      staplelwd=2, boxwex=0.5, staplecol=colo, medbg=colo, boxfill=colo, pch=16,
      ylim=c(0.3,1), 
      xlab="", ylab="", xaxt="n", yaxt="n", xaxs="i",
      main="", axes=FALSE, horiz=TRUE)
  
  axis(1, las=1, cex.axis=2, lwd=1.5, padj=0.25)
  mtext("Recall", 1, line=4.5, cex=1.75)
  title(main="(b)", cex.main=2.5, line=1)
  box(lwd=2)
  abline(v=0.8, lty=2, lwd=2, col=colo)
  
  par(new = TRUE)
  matplot(recall, 9:1, col=colopt, bg=colopt, pch=pchpt, cex=2,
          xlim=c(0.3,1), ylim=c(0.5,9.5),
          xlab="", ylab="", xaxt="n", yaxt="n",
          main="", axes=FALSE)
  
  #c
  par(mar=c(6,1,3,1))
  bxp(boxc, notch=FALSE, outline=FALSE,
      outcol=colo, boxcol=colo, whiskcol=colo, whisklty="solid", whisklwd=2,
      staplelwd=2, boxwex=0.5, staplecol=colo, medbg=colo, boxfill=colo, pch=16,
      ylim=c(0.3,1), 
      xlab="", ylab="", xaxt="n", yaxt="n", xaxs="i",
      main="", axes=FALSE, horiz=TRUE)
  
  axis(1, las=1, cex.axis=2, lwd=1.5, padj=0.25)
  mtext("F1-score", 1, line=4.5, cex=1.75)
  title(main="(c)", cex.main=2.5, line=1)
  box(lwd=2)
  abline(v=0.8, lty=2, lwd=2, col=colo)
  
  par(new = TRUE)
  matplot(f1, 9:1, col=colopt, bg=colopt, pch=pchpt, cex=2,
          xlim=c(0.3,1), ylim=c(0.5,9.5),
          xlab="", ylab="", xaxt="n", yaxt="n",
          main="", axes=FALSE)
  
dev.off()

# Figure S3
rep <- unique(swt$rep)
nrep <- length(rep)

box = list()
length(box) <- nrep
for(k in 1:nrep){
  box[[k]] <- swt$f1.score[swt$rep == rep[k] & swt$labels == "weighted avg"]
}
boxa <- boxplot(box, plot=F, xaxt="n")
for(k in 1:length(box)){
  boxa$stats[1,k]=min(box[[k]])
  boxa$stats[5,k]=max(box[[k]])
}

pdf("figures/FigS3.pdf",width=7.11, height=6, useDingbats=FALSE)

  colo <- "lightgrey"
  
  par(mar=c(5,7,1,1))
  bxp(boxa, notch=FALSE, outline=FALSE,
      outcol=colo, boxcol=colo, whiskcol=colo, whisklty="solid", whisklwd=2,
      staplelwd=2, boxwex=0.5, staplecol=colo, medbg=colo, boxfill=colo, pch=16,
      ylim=c(0.8,0.95), 
      xlab="", ylab="", xaxt="n", yaxt="n", xaxs="i",
      main="", axes=FALSE, horiz=FALSE)
  
  axis(1, at=1:5, labels=1:5, las=1, cex.axis=1.25, font=2)
  axis(2, las=1, cex.axis=1.25, lwd=1.5, padj=0.25)
  mtext("Seed", 1, line=3, cex=1.75)
  mtext("F1-score (weighted avg)", 2, line=4.5, cex=1.75)
  box(lwd=2)
  
dev.off()  

# Figure S4
sites <- list.files(path = "data/boundaries")
sites <- substr(sites, 1, nchar(sites)-5)
n <- length(sites)

sites <- sites[c(1,4,6,7,2,3,5)]

prec <- NULL
recall <- NULL
f1 <- NULL
for(k in 1:n){
 site <- read.csv(paste0("outputs/sites/",
                         tolower(sites[k]), "/",
                         tolower(sites[k]),"_classification_report.csv"))
 site[1:5,] <- site[c(2,1,4,5,3),]
 prec <- cbind(prec, site$precision)
 recall <- cbind(recall, site$recall)
 f1 <- cbind(f1, site$f1)
}
prec[prec==0] <- -1
recall[recall==0] <- -1
f1[f1==0] <- -1

pdf("figures/FigS4.pdf",width=14.14, height=5.08, useDingbats=FALSE)

  colopt <- c("#5F9DFB","#5F9DFB", "#5F9DFB", "#5F9DFB",
              "#F8766D", "#F8766D", "#F8766D")
  pchpt <- 0:6
  
  layout(matrix(c(1,2,3), 1, 3, byrow = TRUE),widths=c(4,2.9,2.9))
  
  #a
  par(mar=c(6,13,3,1))
  matplot(prec, 9:1, col=colopt, bg=colopt, pch=pchpt, cex=2,
          xlim=c(0,1), ylim=c(0.5,9.5),
          xlab="", ylab="", xaxt="n", yaxt="n",
          main="", axes=FALSE)
  axis(1, las=1, cex.axis=2, lwd=1.5, padj=0.25)
  axis(2, at=1:nlbl, labels=lbl, las=2, tick=FALSE, cex.axis=2, font=2, padj=0.5)
  mtext("Precision", 1, line=4.5, cex=1.75)
  title(main="(a)", cex.main=2.5, line=1)
  box(lwd=2)
  abline(v=0.8, lty=2, lwd=2, col="darkgrey")
  
  #b
  par(mar=c(6,1,3,1))
  matplot(recall, 9:1, col=colopt, bg=colopt, pch=pchpt, cex=2,
          xlim=c(0,1), ylim=c(0.5,9.5),
          xlab="", ylab="", xaxt="n", yaxt="n",
          main="", axes=FALSE)  
  axis(1, las=1, cex.axis=2, lwd=1.5, padj=0.25)
  mtext("Recall", 1, line=4.5, cex=1.75)
  title(main="(b)", cex.main=2.5, line=1)
  box(lwd=2)
  abline(v=0.8, lty=2, lwd=2, col="darkgrey")
 
  legend("bottomleft", inset=c(0,-0.01), 
         legend=gsub("_", " ", sites),
         pch=pchpt, col=colopt, pt.bg=colopt,
         bty="n", cex=1.75, xpd=TRUE)
  
  #c
  par(mar=c(6,1,3,1))
  matplot(f1, 9:1, col=colopt, bg=colopt, pch=pchpt, cex=2,
          xlim=c(0,1), ylim=c(0.5,9.5),
          xlab="", ylab="", xaxt="n", yaxt="n",
          main="", axes=FALSE)  
  axis(1, las=1, cex.axis=2, lwd=1.5, padj=0.25)
  mtext("F1-score", 1, line=4.5, cex=1.75)
  title(main="(c)", cex.main=2.5, line=1)
  box(lwd=2)
  abline(v=0.8, lty=2, lwd=2, col="darkgrey")

dev.off()

# Table S2
support <- NULL
for(k in 1:n){
  site <- read.csv(paste0("outputs/sites/",
                          tolower(sites[k]), "/",
                          tolower(sites[k]),"_classification_report.csv"))
  support <- cbind(support, site$support)
}
support <- support[c(2,1,4,5,3),]

tex=paste0(rev(lbl)[1:5], " & ",
           support[,1], " & ",
           support[,2], " & ",
           support[,3], " & ",
           support[,4], " & ",
           support[,5], " & ",
           support[,6], " & ",
           support[,7], " \\")
print(data.frame(tex), row.names = FALSE)

# Figure S

pdf("figures/FigS5.pdf",width=11, height=7, useDingbats=FALSE)

  par(mar=c(5,5,1,1))
  plot(as.numeric(support), as.numeric(prec[1:5,]), 
       pch=16, col="steelblue3", log="xy", cex=2,
       xlab="", ylab="", xaxt="n", yaxt="n",
       main="", axes=FALSE) 
  eaxis(1, at=c(1,10,100,1000))
  eaxis(2, at=c(0.001,0.01,0.1,1))
  mtext("Support", 1, line=3, cex=2)
  mtext("Precision", 2, line=3, cex=2)
  box(lwd=2)

dev.off()




