###############################################################################
# SUMMARY PANEL 
###############################################################################

################################################################################
# 1) Load required packages
################################################################################

library(tidyverse)     # Core data manipulation.
library(ggplot2)      # Main plotting system for all figure panels.
library(dplyr)        # Data filtering, mutation, select, joins, and summaries.
library(sf)          # Simple features for spatial point and polygon data.
library(terra)         # Raster handling for elevation, slope, and hillshade.
library(tidyterra)       # ggplot2 geoms for terra rasters.
library(elevatr)       # Download elevation rasters from elevation datasets.
library(ggnewscale)      # Allows multiple fill scales in the same ggplot.
library(rnaturalearth)    # Country borders and map outlines from Natural Earth.
library(rnaturalearthdata)    # Data package required by rnaturalearth.
library(factoextra)      # PCA visualisation tools, including biplots.
library(cowplot)       # Flexible multi-panel plot arrangement and annotations.


###############################################################################
# 2) Create the color basement for each species and the base theme.
###############################################################################

# Define the species color palette: 
cols <- c(
  "Athene noctua" = "#b67a34",
  "Tyto alba"     = "#7f8fe8"
)

# Then create a shared theme for consistent figure styling: 
base_theme <- theme_minimal(base_size = 10.5, base_family = "serif") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e7e2d9", linewidth = 0.3),
    plot.title = element_text(face = "bold", size = 11.5, color = "black"),
    axis.title = element_text(size = 10.2),
    axis.text = element_text(size = 9.2)
  )


###############################################################################
# 3) Selection of data variables.
###############################################################################

# Keep the two owl species and convert environmental variables to numeric format:
owl2 <- owl %>%
  filter(species %in% c("Athene noctua", "Tyto alba")) %>%
  mutate(
    elevation = as.numeric(elevation),
    tmax_mean_c = as.numeric(tmax_mean_c),
    prec_annual = as.numeric(prec_annual),
    tas_current_october_c = as.numeric(tas_current_october_c),
    tas_future_october_2050_c = as.numeric(tas_future_october_2050_c),
    delta_tas_october_c = as.numeric(delta_tas_october_c),
    NDVI = as.numeric(NDVI),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )


###############################################################################
# 4) Create the Italy map
###############################################################################

# Load the Italy country boundary as an sf object:
italy_sf <- rnaturalearth::ne_countries(
  country = "Italy",
  scale = "medium",
  returnclass = "sf"
)

# Retain only occurrence records with complete geographic coordinates:
map_dat <- owl2 %>%
  drop_na(longitude, latitude, species)

#Convert occurrence coordinates into an sf point object:
pts_sf <- sf::st_as_sf(map_dat, coords = c("longitude", "latitude"), crs = 4326)

# Download a digital elevation model clipped to Italy:
dem_raster <- elevatr::get_elev_raster(
  locations = italy_sf,
  z = 5,
  clip = "locations"
)

# Convert spatial objects to terra format for raster processing:
dem <- terra::rast(dem_raster)
italy_vect <- terra::vect(italy_sf)

# Crop and mask elevation data to the Italian boundary:
dem <- terra::crop(dem, italy_vect)
dem <- terra::mask(dem, italy_vect)

#Derive slope, aspect, and hillshade layers to create terrain relief:
slope  <- terra::terrain(dem, v = "slope", unit = "radians")
aspect <- terra::terrain(dem, v = "aspect", unit = "radians")
hill   <- terra::shade(slope, aspect, angle = 40, direction = 320)

# Rename raster layers:
names(dem)  <- "elevation"
names(hill) <- "hillshade"

# Build the map panel with the relief, the elevation tint, the occurrences, and Italy borders:
graphA <- ggplot() +
  tidyterra::geom_spatraster(data = hill, aes(fill = hillshade), alpha = 0.95, show.legend = FALSE) +
  scale_fill_gradientn(
    colours = c("#fffefe", "#f5f4f1", "#e8e6e0", "#d2cdc3", "#ada596"),
    na.value = NA
  ) +
  ggnewscale::new_scale_fill() +
  tidyterra::geom_spatraster(data = dem, aes(fill = elevation), alpha = 0.90, show.legend = FALSE) +
  scale_fill_gradientn(
    colours = c("#fffdf8", "#f4ecdf", "#e5d4ba", "#c5ab84", "#9a7d58"),
    na.value = NA
  ) +
  ggnewscale::new_scale_color() +
  geom_sf(
    data = pts_sf,
    aes(color = species, shape = species),
    size = 2.0,
    alpha = 0.92,
    stroke = 0
  ) +
  scale_color_manual(values = cols, name = NULL) +
  scale_shape_manual(
    values = c(16, 17),
    name = NULL,
    labels = c("Athene noctua", "Tyto alba")
  ) +
  geom_sf(data = italy_sf, fill = NA, color = "#ffffff", linewidth = 0.90) +
  geom_sf(data = italy_sf, fill = NA, color = "#6e6861", linewidth = 0.55) +
  coord_sf(
    xlim = c(6.3, 18.7),
    ylim = c(36.5, 47.2),
    expand = FALSE
  ) +
  labs(title = "A. Occurrence records across Italy") +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, family = "serif"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.17, 0.16),
    legend.direction = "vertical",
    legend.text = element_text(size = 9.5)
  )


###############################################################################
# 5) Create the PCA-Biplot.
###############################################################################

# Select and rename environmental variables:
vars <- owl2 %>%
  dplyr::select(
    elevation,
    tmax_mean_c,
    prec_annual,
    tas_current_october_c,
    tas_future_october_2050_c,
    delta_tas_october_c,
    NDVI
  ) %>%
  rename(
    Elevation = elevation,
    `Maximum temperature` = tmax_mean_c,
    `Annual precipitation` = prec_annual,
    `Current October temperature` = tas_current_october_c,
    `Projected October temperature (2050)` = tas_future_october_2050_c,
    `October warming (2050-current)` = delta_tas_october_c,
    `Vegetation productivity (NDVI)` = NDVI
  )

# Remove the incomplete and non-informative variables:
complete_rows <- complete.cases(vars)
vars_clean <- vars[complete_rows, ]
owl_clean  <- owl2[complete_rows, ]
vars_clean <- vars_clean[, apply(vars_clean, 2, sd, na.rm = TRUE) > 0]

# Run PCA :
pca <- prcomp(vars_clean, center = TRUE, scale. = TRUE)

#Create the PCA-Biplot:
graphB <- fviz_pca_biplot(
  pca,
  geom.ind = "point",
  habillage = as.factor(owl_clean$species),
  addEllipses = TRUE,
  ellipse.level = 0.95,
  pointsize = 1.35,
  label = "var",
  repel = TRUE,
  col.var = "#2f2f2f",
  palette = c(cols["Athene noctua"], cols["Tyto alba"]),
  select.var = list(contrib = 5)
) +
  labs(title = "B. PCA-Biplot") +
  base_theme +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )


###############################################################################
# 6)  Create the boxplots
###############################################################################

# Reshape and standardize environmental variables:
box_dat <- owl2 %>%
  drop_na(elevation, tmax_mean_c, prec_annual, tas_current_october_c, NDVI) %>%
  dplyr::select(
    species,
    elevation,
    tmax_mean_c,
    prec_annual,
    tas_current_october_c,
    NDVI
  ) %>%
  rename(
    Elevation = elevation,
    `Max temp.` = tmax_mean_c,
    `Annual precip.` = prec_annual,
    `October temp.` = tas_current_october_c,
    NDVI = NDVI
  ) %>%
  pivot_longer(
    cols = -species,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(z_value = as.numeric(scale(value))) %>%
  ungroup()

# Define the plotting order of environmental variables:
var_order <- c("Elevation", "Max temp.", "Annual precip.", "October temp.", "NDVI")

# Apply the custom variable order to the long-format dataset:
box_dat <- box_dat %>%
  mutate(variable = factor(variable, levels = var_order))

# Create boxplots:
graphC <- ggplot(box_dat, aes(x = variable, y = z_value, fill = species)) +
  geom_boxplot(
    position = position_dodge(width = 0.72),
    width = 0.60,
    alpha = 0.86,
    outlier.shape = NA,
    color = "#5a5248",
    linewidth = 0.28
  ) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.10, dodge.width = 0.72),
    size = 0.9,
    alpha = 0.35,
    color = "#2f2f2f",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  labs(
    title = "C. Inter-specific variation across standardized gradients",
    x = NULL,
    y = "z-score",
    fill = NULL
  ) +
  base_theme +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 16, hjust = 1)
  )


###############################################################################
# 7) Create the scatterplot Elevation versus October temperature
###############################################################################

# Set a fixed species order to ensure consistent legend and color mapping:
scatter_dat <- scatter_dat %>%
  mutate(species = factor(species, levels = c("Athene noctua", "Tyto alba")))

# Create the scatterplot, the relationship between October temperature and elevation for both species:
graphD <- ggplot(scatter_dat, aes(x = `October temp.`, y = Elevation, color = species)) +
  geom_point(size = 1.8, alpha = 0.72) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_color_manual(values = cols, drop = FALSE) +
  labs(
    title = "D. Elevation versus October temperature",
    x = "October temperature (°C)",
    y = "Elevation (m)",
    color = NULL
  ) +
  base_theme +
  theme(legend.position = "top")


###############################################################################
# 8) Compose the final figure.
###############################################################################

# Assemble the four panels into a single summary figure:
summary_panel <- ggdraw() +
  draw_plot(graphA, x = 0.00, y = 0.28, width = 0.47, height = 0.67) +
  draw_plot(graphB, x = 0.53, y = 0.58, width = 0.43, height = 0.35) +
  draw_plot(graphC, x = 0.53, y = 0.31, width = 0.43, height = 0.25) +
  draw_plot(graphD, x = 0.00, y = 0.03, width = 1.00, height = 0.25) +
  draw_label(
    "Environmental niche differentiation of Athene noctua and Tyto alba in Italy",
    x = 0.5, y = 0.985, fontface = "bold", fontfamily = "serif", size = 17
  ) +
  draw_label("A", x = 0.01, y = 0.95, fontface = "bold", fontfamily = "serif", size = 15) +
  draw_label("B", x = 0.53, y = 0.95, fontface = "bold", fontfamily = "serif", size = 15) +
  draw_label("C", x = 0.53, y = 0.57, fontface = "bold", fontfamily = "serif", size = 15) +
  draw_label("D", x = 0.01, y = 0.26, fontface = "bold", fontfamily = "serif", size = 15)

x11()
print(summary_panel)


# Save the plot:
ggsave(
  "Figures/Owl_summary_figure.png",
  summary_panel,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "white"
)
