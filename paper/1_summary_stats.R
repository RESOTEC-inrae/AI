library(data.table)
library(dplyr)
library(tidyr)
library(sf)
library(terra)
library(elevatr)
library(xtable)
sf::sf_use_s2(FALSE)

# Names of the tables to compile
names = c("full", "balanced")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# - Table of site characteristics ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Raster of land cover
rast_landcover = terra::rast("data/landcover/U2018_CLC2018_V2020_20u1.tif")

# Raster of precipitation
rast_prec = terra::rast("data/climate/CHELSA_bio12_1981-2010_V.2.1.tif")

# Raster of mean annual temperature
rast_temp = terra::rast("data/climate/CHELSA_bio01_1981-2010_V.2.1.tif")

# List sites
sites <- list.files(path = "data/boundaries")
sites <- substr(sites, 1, nchar(sites)-5)
n <- length(sites)
sites <- sites[c(1,4,6,7,2,3,5)]

# Loop on all sites
for(i in 1:length(sites)){
  
  # Extract boundaries of the site
  boundaries.site.in = terra::vect(
    paste0("data/boundaries/", sites[i], ".gpkg"), layer = "boundaries")
  
  # Extract elevation data for this site
  # - Get raster
  elevation_raster = get_elev_raster(
    locations = sf::st_as_sf(boundaries.site.in), z = 9,  clip = "locations")
  # - Convert to SpatRaster
  elevation_raster = rast(elevation_raster)
  # Extract statistics
  elevation.i = terra::extract(
    elevation_raster, boundaries.site.in, 
    fun = function(x) c(min = min(x, na.rm = TRUE),
                        mean = mean(x, na.rm = TRUE),
                        max = max(x, na.rm = TRUE)))
  # Extract land cover for the site
  land.cover.i = extract(rast_landcover, boundaries.site.in, fun = table) %>%
    # pivot_longer(cols = colnames(.)[2:dim(.)[2]], names_to = "landcover_cat", 
    #              values_to = "landcover_npixel")  %>%
    rename(landcover_cat = LABEL3) %>%
    mutate(
      synthesized_class = case_when(
        # Urban
        landcover_cat %in% c(
          "Continuous urban fabric",
          "Discontinuous urban fabric",
          "Green urban areas", 
          "Industrial or commercial units",
          "Road and rail networks and associated land",
          "Port areas",
          "Airports",
          "Mineral extraction sites",
          "Dump sites",
          "Construction sites",
          "Sport and leisure facilities"
        ) ~ "Urban",
        
        # Agriculture
        landcover_cat %in% c(
          "Non-irrigated arable land",
          "Permanently irrigated land",
          "Rice fields",
          "Vineyards",
          "Fruit trees and berry plantations",
          "Olive groves",
          "Pastures",
          "Annual crops associated with permanent crops",
          "Complex cultivation patterns",
          "Land principally occupied by agriculture, with significant areas of natural vegetation",
          "Agro-forestry areas"
        ) ~ "Agriculture",
        
        # Forest
        landcover_cat %in% c(
          "Broad-leaved forest",
          "Coniferous forest",
          "Mixed forest"
        ) ~ "Forest",
        
        # Grassland/Natural Vegetation
        landcover_cat %in% c(
          "Natural grasslands",
          "Moors and heathland",
          "Sclerophyllous vegetation",
          "Transitional woodland-shrub"
        ) ~ "Grassland/Shrubland",
        
        # Water
        landcover_cat %in% c(
          "Water courses",
          "Water bodies",
          "Coastal lagoons",
          "Estuaries",
          "Sea and ocean"
        ) ~ "Water",
        
        # Wetlands
        landcover_cat %in% c(
          "Inland marshes",
          "Peat bogs",
          "Salt marshes",
          "Salines",
          "Intertidal flats"
        ) ~ "Wetlands",
        
        # Glacier / Snow
        landcover_cat %in% c(
          "Glaciers and perpetual snow"
        ) ~ "Snow",
        
        # Barren/Sparse Vegetation
        landcover_cat %in% c(
          "Beaches, dunes, sands",
          "Bare rocks",
          "Sparsely vegetated areas",
          "Burnt areas"
        ) ~ "Barren/Sparse Vegetation",
        
        # Other/No Data
        landcover_cat == "NODATA" ~ "Other",
        
        TRUE ~ NA_character_)) %>%
    group_by(synthesized_class) %>%
    summarize(sumpixel = sum(count, na.rm = TRUE)) %>%
    ungroup() %>% 
    mutate(prop = sumpixel/sum(sumpixel, na.rm = TRUE), 
           perc = paste0(round(prop, digits = 3)*100, " %")) %>%
    dplyr::select(synthesized_class, perc) %>%
    filter(synthesized_class != "Other") %>%
    mutate(site = sites[i])
  
  # Extract temperature and precipitations
  data.clim.i = data.frame(
    site = sites[i], 
    temperature = as.numeric(extract(rast_temp, boundaries.site.in, fun = "mean")[2]), 
    precipitation = as.numeric(extract(rast_prec, boundaries.site.in, fun = "mean")[2]), 
    elevation = as.numeric(extract(elevation_raster, boundaries.site.in, fun = "mean")[2])) %>%
    mutate(temperature = paste0(round(temperature, digits = 1), " °C"), 
           precipitation = paste0(round(precipitation, digits = 0), ' mm'), 
           elevation = paste0(round(elevation, digits = 0), ' m'))
  
  # Assemble data
  if(i == 1){
    data.clim = data.clim.i
    land.cover = land.cover.i
  }else{
    data.clim = rbind(data.clim, data.clim.i)
    land.cover = rbind(land.cover, land.cover.i)
  } 
  
  
}

# Final formatting
out = land.cover  %>%
  pivot_wider(names_from = "synthesized_class", values_from = "perc") %>%
  replace(is.na(.), "0.0%") %>%
  distinct() %>%
  left_join(data.clim, by = "site") %>%
  mutate(site = gsub("\\_", "\\ ", site)) %>%
  pivot_longer(cols = colnames(.)[2:dim(.)[2]], names_to = "col", values_to = "val") %>%
  mutate(col = case_when(col == "temperature" ~ "Mean temperature", 
                         col == "precipitation" ~ "Annual rainfall", 
                         col == "elevation" ~ "Mean elevation", 
                         TRUE ~ col)) %>%
  pivot_wider(names_from = "site", values_from = "val") %>%
  rename(" " = "col")
colnames(out) = c("", sites)

# Make latex table
dir.create("paper/tables")
print(xtable(out, type = "latex", label = "tablesites",
             caption = paste0("Environmental conditions (mean annual temperature", 
                              ", annual rainfall and mean elevation) and percentage", 
                              " of land cover of the seven study sites")), 
      include.rownames=FALSE, include.colnames = TRUE, size = "\\small",
      hline.after = c(0, 3, dim(out)[1]), caption.placement = "top", align = "c",
      file = "paper/tables/tablesites.tex")

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





