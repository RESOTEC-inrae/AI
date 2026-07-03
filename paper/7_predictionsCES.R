# Packages
library(dplyr)
library(ggplot2)
library(data.table)
library(sf)
library(rnaturalearth)
library(tidyr)
library(cowplot)
sf::sf_use_s2(FALSE)

# Working directory
setwd("~/Nextcloud/RESOTEC/AI_Flickr")
if(!dir.exists("figures/FigS5CES")) dir.create("figures/FigS5CES")

# List sites
sites <- list.files(path = "data/boundaries")
sites <- substr(sites, 1, nchar(sites)-5)
n <- length(sites)

# Load metadata on images
data_images <- read.csv("data/images.csv")

# Initialize the list to store plots
plotlist.out = vector(mode = "list", length = length(sites))
maplist.out = vector(mode = "list", length = length(sites))

# Attribute name to each plot
names(plotlist.out) = sites
names(maplist.out) = sites

# Vector of grids
grid_vec = paste0("grid", c(250, 500, 1000, 2000, 5000))

# Initialize table with statistics per grid
table.stat.grid = data.frame(site = character(0), grid = character(0), 
                             ncells = numeric(0), rsquare = numeric(0), 
                             rmse = numeric(0), mae = numeric(0)) 

# Loop on all sites
for(i in 1:length(sites)){
  
  # Identify site
  site.in = sites[i]
  
  # Load predictions
  data.predict.in_file = paste0("outputs/sites/", tolower(site.in), "/", 
                                tolower(site.in), "_prediction_revue.csv")
  data.predict.in = fread(data.predict.in_file) %>% 
    mutate(image = as.numeric(image)) 
  colnames(data.predict.in) = gsub("ŷ", "u", colnames(data.predict.in))
  
  # Re-format df to convert features into CES
  data.predict.ces.in = data.predict.in %>%
    mutate(u_aesthetic = ifelse(u_human == 0 & u_vegetation == 1, 1, 0), 
           u_recreation = ifelse(u_human == 1 & u_vegetation == 1, 1, 0), 
           u_out = ifelse(u_vegetation == 0, 1, 0), 
           y_aesthetic = ifelse(y_human == 0 & y_vegetation == 1, 1, 0), 
           y_recreation = ifelse(y_human == 1 & y_vegetation == 1, 1, 0), 
           y_out = ifelse(u_vegetation == 0, 1, 0)) %>%
    select(image, site, u_aesthetic, u_recreation, u_out, y_aesthetic, 
           y_recreation, y_out) 
  
  # Metadata for this site
  # - Read file
  data_images.in = subset(data_images, site == tolower(site.in))
  # - Convert to sf
  data_images.in_sf = data_images.in %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  
  # Loop on each grid
  for(j in 1:length(grid_vec)){
    
    # Read grid for this site
    grid.in = st_read(paste0("data/boundaries/", site.in, ".gpkg"), 
                      layer = grid_vec[j], quiet = TRUE)
    
    # Final formatting of the image data
    pred.per.cell = data_images.in_sf %>%
      # Assign each image to a cell of the grid
      st_join(grid.in["idcell"]) %>%
      # Remove geometry
      st_drop_geometry() %>%
      # Add an ID for each interaction
      mutate(id_interaction = paste(owner, substr(date, 1, 10), idcell, sep = "_")) %>%
      # Add information on prediction
      left_join(data.predict.ces.in, by = c("image", "site")) %>%
      # Group predictions by interaction
      group_by(idcell, id_interaction) %>%
      summarize(u_aesthetic = ifelse(any(u_aesthetic), 1, 0), 
                y_aesthetic = ifelse(any(y_aesthetic), 1, 0), 
                u_recreation = ifelse(any(u_recreation), 1, 0), 
                y_recreation = ifelse(any(y_recreation), 1, 0), 
                u_out = ifelse(any(u_out), 1, 0), 
                y_out = ifelse(any(y_out), 1, 0)) %>%
      # Minor formatting
      ungroup() %>% 
      pivot_longer(cols = colnames(.)[3:dim(.)[2]], 
                   names_to = "variable", values_to = "value") %>%
      separate(col = "variable", into = c("type", "category"), sep = "_") %>%
      mutate(type = ifelse(type == "y", "observed", "predicted")) %>%
      pivot_wider(names_from = "type", values_from = "value") %>%
      # Sum by cell
      group_by(idcell, category) %>%
      summarise(predicted = sum(predicted, na.rm = TRUE), 
                observed = sum(observed, na.rm = TRUE))
    

    # Fit model of oobserved vs predicted for plotting
    mod = lm(predicted ~ observed*category, data = pred.per.cell)
    
    # Calculate goodness of fit
    rsquare = 1 - sum((pred.per.cell$observed - pred.per.cell$predicted)^2) / 
      sum((pred.per.cell$observed - mean(pred.per.cell$predicted))^2)
    rmse  = sqrt(mean((pred.per.cell$observed - pred.per.cell$predicted)^2))
    mae = mean(abs(pred.per.cell$observed - pred.per.cell$predicted))
    
    # Fill the stat table
    table.stat.grid = rbind(table.stat.grid, data.frame(
      site = site.in, grid = grid_vec[j], ncells = dim(grid.in)[1], 
      rsquare = rsquare, rmse = rmse, mae = mae))
    
    # Only make the plot if it is the chosen grid
    if(grid_vec[j] == "grid2000"){
      
      # Identify the maximum value in the data (useful to position text in the plot)
      max.val.ij = max(c(pred.per.cell$predicted, pred.per.cell$observed), na.rm = TRUE)
      
      # - Make plot
      plot.in = pred.per.cell %>%
        ungroup() %>%
        mutate(fit = predict(mod, newdata = .), 
               predicted = predicted + rnorm(dim(.)[1], 0, 0.1)) %>%
        ggplot(aes(x = observed, y = predicted, color = category)) + 
        geom_point(alpha = 0.4) + 
        geom_line(aes(y = fit)) + 
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
        xlab("Observed interactions") + 
        ylab("Predicted interactions") +
        scale_color_manual(values = c(
          `aesthetic` = "#008000", `recreation` = "#F48C06", `out` = "#9D0208")) +
        theme(panel.grid = element_line(colour = "grey", linetype = "dotted"), 
              panel.background = element_rect(color = 'black', fill = 'white'), 
              plot.title = element_text(hjust = 0.5, size = 18), 
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 13), 
              legend.text = element_text(size = 18),
              legend.title = element_blank(), 
              legend.key = element_blank()) + 
        geom_text(data = data.frame(observed = 0.05*max.val.ij,
                                    predicted = 0.95*max.val.ij, category = NA),
                  label = paste0("R2 = ", round(rsquare, digits = 2),
                                 "\nRMSE = ", round(rmse, digits = 2)),
                  size = 5, color = "black", hjust = 0, vjust = 0.9) +
        ggtitle(gsub("\\_", "\\ ", site.in))
      
      # - Save to list
      plotlist.out[[i]] = plot.in + theme(legend.position = "none")
      
      # Make a map of observed vs predicted per feature
      map.in = grid.in %>%
        left_join((pred.per.cell %>% 
                     pivot_longer(names_to = "prediction", values_to = "value", 
                                  cols = all_of(c("observed", "predicted"))) %>%
                     mutate(cat_predict = paste0(category, "_", prediction)) %>%
                     select(-category, -prediction) %>%
                     pivot_wider(names_from = "cat_predict", values_from = "value")), 
                  by = "idcell") %>% 
        pivot_longer(names_to = "cat_predict", values_to = "value", cols = all_of(
          colnames(.)[2:(dim(.)[2]-1)])) %>%
        mutate(Interactions = ifelse(is.na(value), 0, value)) %>%
        separate(col = "cat_predict", into = c("category", "prediction"), sep = "_") %>%
        ggplot(aes(geometry = geom, fill = Interactions)) + 
        geom_sf(color = "lightgrey") + 
        facet_grid(prediction ~ category) + 
        scale_fill_gradient(low = "white", high = "#D1495D") +
        theme(panel.grid = element_blank(), 
              panel.background = element_rect(color = 'black', fill = 'white'), 
              plot.title = element_text(hjust = 0.5, size = 18), 
              strip.background = element_blank(),
              strip.text = element_text(size = 15), 
              legend.text = element_text(size = 13),
              legend.title = element_text(size = 15),
              legend.position = "bottom",
              axis.text = element_blank(), 
              axis.ticks = element_blank()) + 
        ggtitle(gsub("\\_", "\\ ", site.in))
      # - Store in list
      maplist.out[[i]] = map.in
      
      # Save map
      ggsave(paste0("figures/FigS5CES/", site.in, ".jpeg"), map.in, width = 30, 
             height = 12, units = "cm", dpi = 600, bg = "white")
    }
    
  }
  
}

# Build final plot
# - Extract legend for the main plot
plot.in_legend = cowplot::get_legend(plot.in)
# - Divide into val / train / test and external
plotlist_train = plotlist.out[c("Carpathians", "French_Alps", "Stubai_Valley", "Vinschgau")]
plotlist_external = plotlist.out[c("Danube", "Dovre", "Sierra_Nevada")]
# - Add legend
plotlist_external$legend = plot.in_legend
# - Assemble plots
plot.out = plot_grid(plot_grid(
  plot_grid(plotlist = plotlist_train, nrow = 1, align = "hv", scale = 0.9), 
  plot_grid(plotlist = plotlist_external, nrow = 1, align = "hv", scale = 0.9), 
  ncol = 1, scale = 0.95, labels = c("(a)", "(b)"), label_size = 20), 
  maplist.out[["Carpathians"]], rel_widths = c(1, 0.5), 
  labels = c("", "(c)"), label_size = 20, nrow = 1)
# - Save plots
ggsave("figures/FigS7.pdf", plot.out, width = 45, 
       height = 17, units = "cm", bg = "white")


# Build plot of statistics per grid
plot.stat.grid = table.stat.grid %>%
  mutate(grid = as.numeric(gsub("grid", "", grid))) %>%
  pivot_longer(names_to = "metric", values_to = "value", 
               cols = all_of(c("rsquare", "rmse", "mae"))) %>%
  mutate(metric = toupper(metric)) %>%
  ggplot(aes(x = grid, y = value, color = site)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~ metric, scale = "free") + 
  theme_bw() + 
  ylab("") + xlab("Grid pixel size (m)")
# - Save plot
ggsave("figures/FigS8.pdf", plot.stat.grid, width = 20, 
       height = 7, units = "cm", bg = "white")


