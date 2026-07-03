# Packages
library(dplyr)
library(sf)
library(tidyr)
library(terra)
library(elevatr)
library(xtable)
sf::sf_use_s2(FALSE)

# Working directory
setwd("~/Nextcloud/RESOTEC/AI_Flickr")

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
    pivot_longer(cols = colnames(.)[2:dim(.)[2]], names_to = "landcover_cat", 
                 values_to = "landcover_npixel")  %>%
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
    summarize(sumpixel = sum(landcover_npixel, na.rm = TRUE)) %>%
    ungroup() %>% 
    mutate(prop = sumpixel/sum(sumpixel, na.rm = TRUE), 
           perc = paste0(round(prop, digits = 3)*100, " %")) %>%
    dplyr::select(synthesized_class, perc) %>%
    pivot_wider(names_from = "synthesized_class", values_from = "perc") %>%
    dplyr::select(-Other) %>%
    mutate(site = sites[i])
   
  # Extract temperature and precipitations
  data.i = data.frame(
    site = sites[i], 
    temperature = as.numeric(extract(rast_temp, boundaries.site.in, fun = "mean")[2]), 
    precipitation = as.numeric(extract(rast_prec, boundaries.site.in, fun = "mean")[2]), 
    elevation = as.numeric(extract(elevation_raster, boundaries.site.in, fun = "mean")[2])) %>%
    mutate(temperature = paste0(round(temperature, digits = 1), " °C"), 
           precipitation = paste0(round(precipitation, digits = 0), ' mm'), 
           elevation = paste0(round(elevation, digits = 0), ' m')) %>%
    left_join(land.cover.i, by = "site")
  
  # Assemble data
  if(i == 1) data = data.i
  else data = rbind(data, data.i)
  
  
}

# Final formatting
out = data %>%
  mutate(site = gsub("\\_", "\\ ", site)) %>%
  pivot_longer(cols = colnames(.)[2:dim(.)[2]], names_to = "col", values_to = "val") %>%
  mutate(col = case_when(col == "temperature" ~ "Mean temperature", 
                         col == "precipitation" ~ "Annual rainfall", 
                         col == "elevation" ~ "Mean elevation", 
                         TRUE ~ col)) %>%
  pivot_wider(names_from = "site", values_from = "val") %>%
  rename(" " = "col")

# Make latex table
print(xtable(out, type = "latex", label = "tablesites",
             caption = paste0("Environmental conditions (mean annual temperature", 
                              ", annual rainfall and mean elevation) and percentage", 
                              " of land cover of the seven study sites")), 
      include.rownames=FALSE, include.colnames = TRUE, size = "\\small",
      hline.after = c(0, 3, dim(out)[1]), caption.placement = "top", align = "c",
      file = "figures/tablesites.tex")
