################################################################################
# EXTRACTION OF ELEVATION DATA IN ITALY AND VISUALIZATION
################################################################################


################################################################################
# 1) Load required packages
################################################################################

library(sf)        # Allows modern spatial data handling (simple features).
library(elevatr)   # Used to download elevation data.
library(raster)    # Allows raster data manipulation (maps).
library(ggplot2)   # Allows data visualization.
library(rnaturalearth) # # Provides maps and natural geographic data of the world.

# Disable s2 geometry engine (can avoid issues in some spatial operations).
sf_use_s2(FALSE)


################################################################################
# 2) Load Italy boundaries.
################################################################################

# Retrieve country borders from Natural Earth : 
Italy <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Italy"
)

################################################################################
# 3) Download elevation data.
################################################################################

# z controls resolution (higher = more detail but slower)
elevation_Italy <- get_elev_raster(Italy, z = 8)

# Quick visualization of the elevation raster
x11()
plot(elevation_Italy)
Sys.sleep(3)

################################################################################
# 4) Prepare sampling points.
################################################################################
# Dataset contains:
# - longitude
# - latitude

# Convert coordinates into a spatial object (SpatialPoints format):
spatial_points <- SpatialPoints(
  coords = matrix_full_eco[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


################################################################################
# 5) Extract elevation values.
################################################################################

# Extract raster values at each point location :
elevation <- raster::extract(elevation_Italy, spatial_points)


################################################################################
# 6) Add elevation to the dataset.
################################################################################
matrix_full_eco_elev <- data.frame(
  matrix_full_eco,
  elevation = elevation
)

################################################################################
# 7) Visualization of elevation distribution of species.
################################################################################

# Compare elevation distributions across climate categories: 
p3 <- ggplot(matrix_full_eco_elev, aes(x = elevation, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by Climate",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()

# Display the plot
print(p3)
Sys.sleep(3)


#Visualization of the elevation distribution differences by species.
p_species_elev <- ggplot(
  matrix_full_eco_elev,
  aes(x = species, y = elevation, fill = species)
) +
  
  # Boxplots
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  
  # Individual observations
  geom_jitter(
    width = 0.2,
    alpha = 0.4,
    size = 1
  ) +
  
  labs(
    title = "Elevation Distribution by Species",
    x = "Species",
    y = "Elevation (m)"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none"
  )

# Display the plot
print(p_species_elev)


