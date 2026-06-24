# Working directory
setwd("~/Nextcloud INRAE/RESOTEC/AI_Flickr")

# Minigrid search (Table S1)
minigrid <- read.csv("outputs/minigrid_results.csv")
minigrid <- minigrid[minigrid$labels == "weighted avg",]
minigrid$precision <- round(minigrid$precision, digits = 2)
minigrid$recall <- round(minigrid$recall, digits = 2)
minigrid$f1.score <- round(minigrid$f1.score, digits = 2)

mglength <- aggregate(minigrid$f1.score , 
                      list(minigrid$backbone, 
                           minigrid$batch_size,
                           minigrid$learning_rate), 
                      length)
colnames(mglength) <- c("Backbone", "Batch", "Learning", "NbRuns")

all <- expand.grid(
  Backbone = unique(mglength$Backbone),
  Batch = unique(mglength$Batch),
  Learning = unique(mglength$Learning)
)
existing <- unique(mglength[, c("Backbone", "Batch", "Learning")])
missing <- all[!do.call(paste, all) %in% 
               do.call(paste, existing), ]
colnames(missing) <- c("Backbone", "Batch", "Learning")

mglength <- rbind(mglength, data.frame(missing, NbRuns=0))
mglength <- mglength[order(mglength$Backbone, 
                           mglength$Batch, 
                           mglength$Learning),]

mgmean <- aggregate(minigrid[,6:8], 
                    list(minigrid$backbone, 
                         minigrid$batch_size,
                         minigrid$learning_rate), 
                    mean)
colnames(mgmean) <- c("Backbone", "Batch", "Learning", "Precision", "Recall", "F1")
mgmean <- rbind(mgmean, data.frame(missing, Precision="-", Recall="-", F1="-"))
mgmean <- mgmean[order(mgmean$Backbone, 
                       mgmean$Batch, 
                       mgmean$Learning),]

mgmin <- aggregate(minigrid[,6:8], 
                    list(minigrid$backbone, 
                         minigrid$batch_size,
                         minigrid$learning_rate), 
                    min)
colnames(mgmin) <- c("Backbone", "Batch", "Learning", "Precision", "Recall", "F1")
mgmin <- rbind(mgmin, data.frame(missing, Precision="-", Recall="-", F1="-"))
mgmin <- mgmin[order(mgmin$Backbone, 
                     mgmin$Batch, 
                     mgmin$Learning),]


mgmax <- aggregate(minigrid[,6:8], 
                   list(minigrid$backbone, 
                        minigrid$batch_size,
                        minigrid$learning_rate), 
                   max)
colnames(mgmax) <- c("Backbone", "Batch", "Learning", "Precision", "Recall", "F1")
mgmax <- rbind(mgmax, data.frame(missing, Precision="-", Recall="-", F1="-"))
mgmax <- mgmax[order(mgmax$Backbone, 
                     mgmax$Batch, 
                     mgmax$Learning),]

tex=paste0(gsub("_", "\\\\_", mglength$Backbone), " & ",
           mglength$Batch, " & ",
           mglength$Learning, " & ",
           mgmean$Precision, " [", 
           mgmin$Precision, ", ", 
           mgmax$Precision, "] & ",
           mgmean$Recall, " [", 
           mgmin$Recall, ", ", 
           mgmax$Recall, "] & ",
           mgmean$F1, " [", 
           mgmin$F1, ", ", 
           mgmax$F1, "] & ",
           mglength$NbRuns, " \\")
print(data.frame(tex), row.names = FALSE)

# Evolution train/val loss/F1 during training for the best model [Figure S2]
evol <- read.csv("outputs/best_model_curves.csv")

pdf("figures/FigS2.pdf", width = 13, height = 5.4, useDingbats = FALSE)

  colo <- c("#2F74B1", "#FD842F")

  par(mfrow = c(1,2))

  #a
  par(mar=c(5,6,1,1))
  plot(evol$epoch, evol$train_loss, axes=FALSE, xlab="", ylab="",
       xlim = c(1,20), ylim = c(0.45,0.65),
       type="b", col=colo[1], pch=16, lwd=1, cex=1.2)
  par(new=TRUE)
  plot(evol$epoch, evol$val_loss, axes=FALSE, xlab="", ylab="",
       xlim = c(1,20), ylim = c(0.45,0.65),
       type="b", col=colo[2], pch=17, lwd=1, cex=1.2)
  
  axis(1, las=1, cex.axis=1.25)
  axis(2, las=1, cex.axis=1.25)
  mtext("Epoch", 1, line=3.25, cex=2)
  mtext("Loss", 2, line=4.25, cex=2)
  box(lwd=1.5)
  
  legend("topleft", inset=c(-0.35,-0.1), legend="(a)", bty="n", cex=2, xpd=TRUE)
  
  #b
  par(mar=c(5,6,1,1))
  plot(evol$epoch, evol$train_f1, axes=FALSE, xlab="", ylab="",
       xlim = c(1,20), ylim = c(0.6,1),
       type="b", col=colo[1], pch=16, lwd=1, cex=1.2)
  par(new=TRUE)
  plot(evol$epoch, evol$val_f1, axes=FALSE, xlab="", ylab="",
       xlim = c(1,20), ylim = c(0.6,1),
       type="b", col=colo[2], pch=17, lwd=1, cex=1.2)
  
  axis(1, las=1, cex.axis=1.25)
  axis(2, las=1, cex.axis=1.25)
  mtext("Epoch", 1, line=3.25, cex=2)
  mtext("F1-score", 2, line=3.5, cex=2)
  box(lwd=1.5)
  
  legend("topleft", inset=c(-0.35,-0.1), legend="(b)", bty="n", cex=2, xpd=TRUE)
  legend("bottomright", inset=c(0,0), 
         legend=c("Train", "Validation"),
         pch=c(16,17), lwd=1, col=colo,
         bty="n", cex=1.5, xpd=TRUE)
  
dev.off()



