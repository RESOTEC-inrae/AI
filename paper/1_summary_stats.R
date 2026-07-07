library(data.table)
library(dplyr)
library(tidyr)

# Names of the tables to compile
names = c("full", "balanced")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# - Table of support per site ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Initalize the list of outputs
list.table = vector(mode = "list", length = 2)
names(list.table) = names

# Loop on names
for(name in names){
  
  # List sites
  sites <- list.files(path = "data/boundaries")
  sites <- substr(sites, 1, nchar(sites)-5)
  n <- length(sites)
  
  # Reorder sites
  sites <- sites[c(1,4,6,7,2,3,5)]
  
  # Load data
  tab <- read.csv("data/images.csv")
  train <- read.csv(paste0("outputs/train/train_prediction_revue_", name, ".csv"))
  val <- read.csv(paste0("outputs/val/val_prediction_revue_", name, ".csv"))
  test <- read.csv(paste0("outputs/test/test_prediction_revue_", name, ".csv"))
  external <- read.csv(paste0("outputs/external/external_prediction_revue_", name, ".csv")) 
  
  
  # Prepare output
  res <- as.data.frame(matrix(0,7,5))
  colnames(res) <- c("Site", "Train", "Validation", "Test", "External")
  res$Site <- gsub("_", " ", sites)
  
  # Loop over site
  for(k in 1:n){
    if(k <= 4){
      res[k,2] <- sum(train$site == sites[k])
      res[k,3] <- sum(val$site == sites[k])
      res[k,4] <- sum(test$site == sites[k])
    }else{
      res[k,5] <- sum(external$site == sites[k])
    }
  }
  
  # Compute Total
  res$Total <- apply(res[,-1], 1, sum)
  sum(res$Total == table(tab$site)) # 7
  res <- rbind(res, res[,6])
  res[8,1] <- "Total"
  res[8,-1] <- apply(res[1:7,-1], 2, sum)
  
  # Add to final list
  eval(parse(text = paste0("list.table$", name, " = res")))
  
}

# Compile final table
table.out = list.table$balanced
for(i in 1:dim(table.out)[1]) table.out[i, 2:6] = paste0(
  table.out[i, 2:6], " [", list.table$full[i, 2:6], "]")
table.out[table.out == "0 [0]"] = "-"
table.out


# print with tex format
tex=paste0("\textbf{", table.out$Site, "}", " & ",
           table.out$Train, " & ",
           table.out$Validation, " & ",
           table.out$Test, " & ",
           table.out$External, " & ",
           table.out$Total, " \\")
print(data.frame(tex), row.names = FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# - Table of support per label ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Initalize the list of outputs
list.table = vector(mode = "list", length = 2)
names(list.table) = names

# Loop on names
for(name in names){
  
  # Read data
  train <- read.csv(paste0("outputs/train/train_classification_report_", name, ".csv"))
  val <- read.csv(paste0("outputs/val/val_classification_report_", name, ".csv"))
  test <- read.csv(paste0("outputs/test/test_classification_report_", name, ".csv"))
  external <- read.csv(paste0("outputs/external/external_classification_report_", name, ".csv"))
  
  # Compile table
  support <- data.frame(Label = train$labels[1:5], 
                        Train = train$support[1:5], 
                        Validation = val$support[1:5],
                        Test = test$support[1:5],
                        External = external$support[1:5])
  support <- support[order(support$Label),]
  
  # Add to final list
  eval(parse(text = paste0("list.table$", name, " = support")))
  
}

# Compile final table
table.out = list.table$balanced
for(i in 1:dim(table.out)[1]) table.out[i, 2:5] = paste0(
  table.out[i, 2:5], " [", list.table$full[i, 2:5], "]")

# Print for latex table
tex=paste0(table.out$Label, " & ",
           table.out$Train, " & ",
           table.out$Validation, " & ",
           table.out$Test, " & ",
           table.out$External, " \\")
print(data.frame(tex), row.names = FALSE)





