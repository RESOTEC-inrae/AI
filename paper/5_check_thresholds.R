library(dplyr)
library(tidyr)
library(ggplot2)

# Parameter
split <- c("val", "test", "external")
classproba <- 1:5
classproba <- classproba[c(2,1,4,5,3)]
classpred <- c(4,7,10,13,16)
classpred <- classpred[c(2,1,4,5,3)]

# Load threshold
th <- read.csv("outputs/thresholds_balanced.csv")
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
                              "_prediction_revue_balanced.csv"))[, classpred[i]]
    p[[i]][[j]] <- read.csv(paste0("outputs/", 
                              split[j], 
                              "/", 
                              split[j], 
                              "_proba_balanced.csv"))[, classproba[i]]
    
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
    
    resij$split = split[j]
    resij$feature = thnames[i]
    res[[i]][[j]] <- resij
    
    if(i == 1 & j == 1) df = resij
    else df = rbind(df, resij)
  }
}

# Make plot
plot = df %>%
  pivot_longer(names_to = "metric", values_to = "value", 
               cols = c("precision", "recall", "f1")) %>%
  ggplot(aes(x = thx, y = value, color = metric)) + 
  geom_line() + 
  facet_grid(split ~ feature) + 
  xlab("Treshold") + ylab("Performance metric") +
  theme(panel.grid = element_line(colour = "grey", linetype = "dotted"), 
        panel.background = element_rect(color = 'black', fill = 'white'), 
        strip.background = element_rect(color = 'black', fill = 'white'),
        strip.text = element_text(size = 16), 
        axis.title = element_text(size = 16), 
        legend.key = element_blank(), 
        legend.text = element_text(size = 16), 
        legend.title = element_blank()) 

# Save plot
ggsave("paper/figures/Fig_supp_tresholds.pdf", plot, width = 29, 
       height = 13, units = "cm", dpi = 600, bg = "white")

