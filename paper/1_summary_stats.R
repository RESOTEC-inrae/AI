# Working directory
setwd("~/Nextcloud INRAE/RESOTEC/AI_Flickr")

# Load data
tab <- read.csv("data/images.csv")
train <- read.csv("outputs/train/train_prediction_revue.csv")
val <- read.csv("outputs/val/val_prediction_revue.csv")
test <- read.csv("outputs/test/test_prediction_revue.csv")
holdout <- read.csv("outputs/holdout/holdout_prediction_revue.csv") 
external <- read.csv("outputs/external/external_prediction_revue.csv") 

# List sites
sites <- list.files(path = "data/boundaries")
sites <- substr(sites, 1, nchar(sites)-5)
n <- length(sites)

# Reorder sites
sites <- sites[c(1,4,6,7,2,3,5)]

# Prepare output
res <- as.data.frame(matrix(0,7,6))
colnames(res) <- c("Site", "Train", "Validation", "Test", "Holdout", "External")
res$Site <- gsub("_", " ", sites)

# Loop over site
for(k in 1:n){
  if(k <= 4){
    res[k,2] <- sum(train$site == tolower(sites[k]))
    res[k,3] <- sum(val$site == tolower(sites[k]))
    res[k,4] <- sum(test$site == tolower(sites[k]))
    res[k,5] <- sum(holdout$site == tolower(sites[k]))
  }else{
    res[k,6] <- sum(external$site == tolower(sites[k]))
  }
}

# Compute Total
res$Total <- apply(res[,-1], 1, sum)
sum(res$Total == table(tab$site)) # 7

res <- rbind(res, res[,7])
res[8,1] <- "Total"
res[8,-1] <- apply(res[1:7,-1], 2, sum)

res[res == 0] <- "-"

# Support by split and site [Table 2]
tex=paste0("\textbf{", res$Site, "}", " & ",
           res$Train, " & ",
           res$Validation, " & ",
           res$Test, " & ",
           res$Holdout, " & ",
           res$External, " & ",
           res$Total, " \\")
print(data.frame(tex), row.names = FALSE)

# Support by label [Table 3]
train <- read.csv("outputs/train/train_classification_report.csv")
val <- read.csv("outputs/val/val_classification_report.csv")
test <- read.csv("outputs/test/test_classification_report.csv")
holdout <- read.csv("outputs/holdout/holdout_classification_report.csv")
external <- read.csv("outputs/external/external_classification_report.csv")

support <- data.frame(Label = train$labels[1:5], 
                      Train = train$support[1:5], 
                      Validation = val$support[1:5],
                      Test = test$support[1:5],
                      Holdout = holdout$support[1:5],
                      External = external$support[1:5])

support <- support[order(support$Label),]

tex=paste0(support$Label, " & ",
           support$Train, " & ",
           support$Validation, " & ",
           support$Test, " & ",
           support$Holdout, " & ",
           support$External, " \\")
print(data.frame(tex), row.names = FALSE)





