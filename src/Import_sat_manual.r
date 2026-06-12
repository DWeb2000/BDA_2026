################################################################################
# EXTRACT SATELLITE VALUES AT POINT LOCATIONS, AND ADD THEM TO THE MAIN MATRIX.
################################################################################

################################################################################
# 1) Load required packages
################################################################################

library(luna) # Provides tools to access and process satellite and remote sensing data.
library(MODIStsp) # Used to download and preprocess MODIS satellite imagery time series.

library(appeears) # Allows access to NASA AppEEARS satellite and environmental data services.
library(terra) # Used to manipulate, analyze, and extract spatial raster and vector data.
library(sf) # Used to manipulate vector spatial data (points, lines, polygons).
library(rnaturalearth) # Provides maps and natural geographic data of the world.
library(ggplot2) # Used to create graphs and visualizations of data.
library(dplyr) # Facilitates the manipulation, filtering, and organization of dataframes.


################################################################################
# 2) Export the Italy polygon for manual upload in AppEEARS.
################################################################################

#At first, create the borders of Italy:
Italy_sf <- ne_countries(
  scale = "medium",
  country = "Italy",
  returnclass = "sf"
)

#Create a directory where to put the Italy polygon :
dir.create("./data/appeears_manual_download", showWarnings = FALSE)

#Create a geojson file of Italy delimitation in the directory data:
st_write(
  Italy_sf,
  "./data/italy.geojson",
  delete_dsn = TRUE
)

#Check and vizualise the Italy map :
plot(st_geometry(Italy_sf), col = "lightgray", main = "Italy")
Sys.sleep(3)

################################################################################
# 3) Following manual steps required in AppEEARS to input satellite data after.  
################################################################################
# 1. Open the AppEEARS website
# 2. Create an AREA request
# 3. Upload the file: .data/italy.geojson
# 4. Select product: MOD13Q1.061
# 5. Select layer: NDVI
# 6. Select the desired date range (october 2025 --> 01.10.2025 - 16.10.2025)
# 7. Choose GeoTIFF as output format 
# 8. SelectGeographic projection
# 8. Submit the task
# 9. Download the resulting NDVI raster manually on the laptop 
# 10. Save it in the folder: .data/appeears_manual_download
################################################################################


################################################################################
# 4) Read the manually downloaded NDVI raster.
################################################################################

#Define the path for taking the required tif file:
manual_path <- "./data/appeears_manual_download"

# List all tif files in the folder to check:
manual_tif <- list.files(
  manual_path,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

print(manual_tif)


# Read the first raster:
ndvi_raster <- rast(manual_tif[1])

# Check raster information:
print(ndvi_raster)

# Plot the raster:
plot(ndvi_raster, main = "Downloaded Italy NDVI raster")
Sys.sleep(3)

################################################################################
# 5) Clip the raster to the exact Italy border.
################################################################################

Italy_vect <- vect(Italy_sf)


# Reproject the Italy polygon to the raster CRS:
Italy_vect <- project(Italy_vect, crs(ndvi_raster))

# Crop and mask :
ndvi_italy <- crop(ndvi_raster, Italy_vect)
ndvi_italy <- mask(ndvi_italy, Italy_vect)


# Plot the clipped raster :
plot(ndvi_italy, main = "NDVI raster clipped to Italy")
plot(Italy_vect, add = TRUE, border = "black", lwd = 1)
Sys.sleep(3)

################################################################################
# 6) Convert the sampling table to spatial points.
################################################################################
# data frame is called matrix_full_eco_elev_climat
# Contains : longitude and latitude columns.

#transform the data table containing the geographic coordinates of the species into geographic points:
points_vect <- vect(
  matrix_full_eco_elev_climat,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Reproject the localisation points to the raster CRS:
points_vect <- project(points_vect, crs(ndvi_italy))

# Plot all the localisation points on top of the raster:
plot(ndvi_italy, main = "Sampling points over NDVI raster")
plot(points_vect, add = TRUE, col = "red", pch = 16)
Sys.sleep(3)

################################################################################
# 7) Extract NDVI values at point locations.
################################################################################
#Extract the raster values ​​at the positions of the geographic points contained in points_vect:
ndvi_values <- terra::extract(ndvi_italy, points_vect)

#Check extracted values
head(ndvi_values)


################################################################################
# 8) Add NDVI values to the main matrix.
################################################################################
# Take the second column of ndvi_values, because the second column contains the extracted raster value.
matrix_full_eco_elev_climat$NDVI <- ndvi_values[, 2]

# Check of the updated table
head(matrix_full_eco_elev_climat)
nrow(matrix_full_eco_elev_climat)
str(matrix_full_eco_elev_climat)


################################################################################
# 9) Control plot.
################################################################################

p5 <- ggplot(matrix_full_eco_elev_climat, aes(x = NDVI, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "NDVI Distribution by Climate",
    x = "NDVI",
    y = "Density"
  ) +
  theme_minimal()


print(p5)
Sys.sleep(3)


#Control plot NDVI by species.
p_species <- ggplot(
  matrix_full_eco_elev_climat,
  aes(x = species, y = NDVI, fill = species)
) +
  
  # Boxplot
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  
  # Points individuels
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
  
  labs(
    title = "NDVI values by owl species",
    x = "Species",
    y = "NDVI"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none"
  )

print(p_species)
################################################################################
# 10) Export final matrix to csv file. 
################################################################################
write.csv(matrix_full_eco_elev_climat,"owl_matrix_full.csv",row.names = FALSE)
