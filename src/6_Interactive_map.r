###############################################################################
# INTERACTIVE PLOT
###############################################################################


################################################################################
# 1) Load required packages
################################################################################

library(sf)     # Simple features for spatial point and polygon data.
library(dplyr)      # Data filtering, mutation, select, joins, and summaries.
library(rnaturalearth)  # Country borders and map outlines from Natural Earth.
library(elevatr)    # Download elevation rasters from elevation datasets.
library(raster)   # Work with raster data, especially grid-based spatial and elevation data.
library(rayshader)  # Create 3D visualizations.
library(rgl)    # Render interactive 3D graphics using OpenGL.


################################################################################
# 2) Preparation of data and a map of Italy.
################################################################################


# Select the two species and the required columns (longitude and latitude):
owl_map <- owl %>%
  dplyr::filter(species %in% c("Athene noctua", "Tyto alba")) %>%
  dplyr::select(species, longitude, latitude) %>%
  dplyr::mutate(
    longitude = as.numeric(longitude),
    latitude  = as.numeric(latitude)
  ) %>%
  dplyr::filter(
    !is.na(species),
    !is.na(longitude),
    !is.na(latitude)
  )


# Define the colour for each species:
cols <- c(
  "Athene noctua" = "#e5a454",
  "Tyto alba"     = "#9198f1"
)

# Download the outline of Italy:
italy <- ne_countries(scale = "medium", country = "Italy", returnclass = "sf")
italy <- st_make_valid(italy)

# Download the topographic data:
dem <- get_elev_raster(locations = italy, z = 5, clip = "locations")

# Convert to a matrix:
elmat <- raster_to_matrix(dem)

# Add the relief texture:
tex <- height_shade(
  elmat,
  texture = colorRampPalette(c("#5c7a3d", "#88a65e", "#b8b08d", "#d9d9d9"))(256)
) %>%
  add_shadow(ray_shade(elmat, zscale = 45), 0.25) %>%
  add_shadow(ambient_shade(elmat), 0.15)



################################################################################
# 3) Creation of the interactive plot.
################################################################################

# Open the 3D Viewport:
plot_3d(
  tex,
  elmat,
  zscale = 100,
  solid = TRUE,
  soliddepth = -max(elmat, na.rm = TRUE) / 25,
  solidcolor = "#bebebe",
  shadow = TRUE,
  background = "#ededed",
  windowsize = c(1000, 900),
  zoom = 0.75,
  phi = 40,
  theta = -20
)

# Add a title
par3d(family = "serif", cex = 1.2)

title3d(
  main = "Topographic distribution of Athene noctua and Tyto alba in Italy",
  cex = 1.2
)

# Add the locations of each species to the map:
for(sp in unique(owl_map$species)) {
  
  dat_sp <- owl_map %>% filter(species == sp)
  
  render_points(
    extent = dem,
    heightmap = elmat,
    lat = dat_sp$latitude,
    long = dat_sp$longitude,
    altitude = NULL,
    offset = 2,
    zscale = 100,
    color = cols[sp],
    size = 5,
    clear_previous = FALSE
  )
}

# Save the plot: 
#render_snapshot("owl_italy_3d.png", clear = FALSE)
