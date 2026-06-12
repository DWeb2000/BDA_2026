###############################################################################
# ADDING ECOSYSTEM DATA TO SPECIES OCCURRENCE COORDINATES
###############################################################################

###############################################################################
# 1) Load required packages
###############################################################################


library(raster) #Used to read and manipulate raster files.
library(sf) #Used to handle vector spatial data.
library(rnaturalearth) #Used to download country boundaries.
library(ggplot2) # Used to create graphs.


###############################################################################
# 2) Load the ecosystem raster.
###############################################################################

#Import the document "WorldEcosystem.tif" on the computer.

# Define the path to the GeoTIFF file:
file_path <- "./data/WorldEcosystem.tif"

# Read the raster layer
# This raster contains ecosystem categories coded as numeric values:
ecosystem_raster <- raster(file_path)

# Display basic information about the raster:
print(ecosystem_raster)

#plot the full raster to verify the visual:
plot(ecosystem_raster, main = "Original Ecosystem Raster")

###############################################################################
# 3) Load the boundary of Italy.
###############################################################################

# Download the country boundary as an sf object
Italy <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Italy"
)

# Plot the country boundary, to verify the visual of exact map:
plot(st_geometry(Italy), main = "Boundary of Italy")

###############################################################################
# 4) Crop and mask the raster to Italy.
###############################################################################

# crop() keeps only the rectangular extent around Italy
r2 <- crop(ecosystem_raster, extent(Italy))

# mask() keeps only the pixels that fall inside the country boundary
ecosystem_Italy <- mask(r2, Italy)

# Plot the cropped and masked raster, to visualize it in Italy:
plot(ecosystem_Italy, main = "Ecosystem Raster Restricted to Italy")
Sys.sleep(3)

###############################################################################
# 5) Convert species coordinates into spatial points.
###############################################################################

# matrix_full is the data frame containing at least:
# - longitude
# - latitude
# - species


# Verify the structure:
head(matrix_full)
matrix_full


# Convert the coordinate columns into spatial points: 
# The CRS used here is WGS84, which is the standard geographic coordinate system
spatial_points <- SpatialPoints(
  coords = matrix_full[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Add the occurrence points of the 2 species on top of the ecosystem map
plot(ecosystem_Italy, main = "Species Occurrences on Ecosystem Map")
plot(spatial_points, add = TRUE, pch = 16, cex = 1.2)
Sys.sleep(3)

###############################################################################
# 6) Extract ecosystem values at each occurence point.
###############################################################################

# extract() function retrieves the raster value at the location of each point.
# Each point receives the ecosystem code of the raster cell where it falls.
eco_values <- raster::extract(ecosystem_Italy, spatial_points)

# Check of the extracted values
head(eco_values)

###############################################################################
# 7) Add the extracted ecosystem values to the original data frame. 
###############################################################################

# Create a new data frame by adding the extracted ecosystem values
matrix_full_eco <- data.frame(matrix_full, eco_values)

# Inspect the result
head(matrix_full_eco)
matrix_full_eco


###############################################################################
# 8) Load the ecosystem metadata table.
###############################################################################

# This metadata table links the numeric raster code to descriptive ecosystem names.
metadata_eco <- read.delim("./data/WorldEcosystem.metadata.tsv")

# Inspect the metadata table :
head(metadata_eco)

###############################################################################
# 9) Merge the extracted values with the metadata. 
###############################################################################

# Merge the occurrence table with the metadata table.
# by.x = "eco_values" means the ecosystem code in our occurrence table.
# by.y = "Value" means the corresponding code column in the metadata table.
matrix_full_eco <- merge(
  matrix_full_eco,
  metadata_eco,
  by.x = "eco_values",
  by.y = "Value"
)

# Inspect the enriched table
head(matrix_full_eco)
nrow(matrix_full_eco)
str(matrix_full_eco)



###############################################################################
# 10) Visualize the number of observations per climate category and species. 
###############################################################################

# Bar plot showing how many observations of each species are found in each climate category.
p2 <- ggplot(matrix_full_eco, aes(x = Climate_Re, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Count of Observations of Each Species by Climate",
    x = "Climate category",
    y = "Number of observations"
  ) +
  theme_minimal()

# Display the plot
print(p2)
