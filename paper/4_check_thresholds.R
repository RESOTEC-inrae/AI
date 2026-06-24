# Working directory
setwd("~/Nextcloud INRAE/RESOTEC/AI_Flickr")

# Parameter
split <- c("val", "test", "holdout", "external")
classproba <- 1:5
classproba <- classproba[c(2,1,4,5,3)]
classpred <- c(4,7,10,13,16)
classpred <- classpred[c(2,1,4,5,3)]

# Load threshold
th <- read.csv("outputs/thresholds.csv")
th <- as.numeric(as.matrix(th))
th <- th[c(2,1,4,5,3)]
thnames <- c("anthropic", "human", "rock", "snow", "vegetation")

# Load y & proba
y <- list()
p <- list()
for(i in 1:length(classproba)){
  
  y[[i]] <- list()
  p[[i]] <- list()
  
  for(j in 1:length(split)){
    
    y[[i]][[j]] <- read.csv(paste0("outputs/", 
                              split[j], 
                              "/", split[j], 
                              "_prediction_revue.csv"))[, classpred[i]]
    p[[i]][[j]] <- read.csv(paste0("outputs/", 
                              split[j], 
                              "/", 
                              split[j], 
                              "_proba.csv"))[, classproba[i]]
    
  }
}



# Compute metrics 
thx <- seq(0,1,0.001)
res <- list()
for(i in 1:length(classproba)){
  
  res[[i]] <- list()
  
  for(j in 1:length(split)){
    
    resij <- data.frame(thx, precision=0, recall=0, f1=0)
    
    yij <- y[[i]][[j]]
    pij <- p[[i]][[j]]
    
    for(k in 1:length(thx)){
      
      yijk <- (pij >= thx[k])*1
      tp <- sum(yijk==1 & yij==1)
      fp <- sum(yijk==1 & yij==0)
      tn <- sum(yijk==0 & yij==0)
      fn <- sum(yijk==0 & yij==1)
      
      precision <- tp / (tp + fp)
      recall <- tp / (tp + fn)
      f1 <- 2*tp / (2*tp + fp + fn)
      
      resij[k,] <- c(thx[k], precision, recall, f1)
      
    }
    
    res[[i]][[j]] <- resij
    
  }
}
  
# Figure S  
pdf("figures/FigS.pdf", width = 14, height = 11, useDingbats = FALSE)

  layout(matrix(1:20, 5, 4, byrow=TRUE), 
         width=c(1.15,1,1,1), 
         height=c(1,1,1,1,1.32))
  
  for(i in 1:length(classproba)){
    for(j in 1:length(split)){
      
      if(i<5 & j>1){
        par(mar=c(2,3,2,1))
      }
      if(i<5 & j==1){
        par(mar=c(2,7,2,1))
      }
      if(i==5 & j>1){
        par(mar=c(7,3,2,1))
      }
      if(i==5 & j==1){
        par(mar=c(7,7,2,1))
      }
      
      matplot(res[[i]][[j]][,1], res[[i]][[j]][,-1], type="l", lty=1, lwd=2,
              col=c("#CC5F6B","#7DCB61","#5393E0"), 
              axes=FALSE, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))
      abline(v=th[i], lwd=2, lty=2, col="lightgray")
      
      title(paste0(thnames[i], " & ", split[j]))
      axis(1, las=1, cex.axis=1.25)
      axis(2, las=1, cex.axis=1.25)
      box(lwd=1.5)
      
      if(i==3 & j==1){
        mtext("Performance metric", 2, line=4, cex=2)
      }
      if(i==5 & j==2){
        mtext("Threshold", 1, line=4.5, cex=2, adj=1.65)
      }
      
      if(i==5 & j==4){
        legend("bottomleft", inset=c(0,0), 
               legend=c("Precision", "Recall", "F1"),
               lty=1, lwd=2, col=c("#CC5F6B","#7DCB61","#5393E0"),
               bty="n", cex=1.5, xpd=TRUE)
      }

    }  
  }  

dev.off()  

