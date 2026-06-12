###############################################################################
# BIODIVERSITY DATASET CONSTRUCTION - 1ST STEP
# Species: Tyto alba & Athene noctua
# Country: Italy
###############################################################################


###############################################################################
# 1) Required packages
###############################################################################

library(rgbif) # Allows to download and manage biodiversity data from the GBIF database.
library(rnaturalearth) # Provides maps and natural geographic data of the world.
library(ggplot2) # Used to create graphs and visualizations of data.
library(rinat) # Allows to obtain species observations from iNaturalist
library(raster) # Used to manipulate and analyze raster data (maps).
library(dplyr) # Facilitates the manipulation, filtering, and organization of dataframes.
library(sf) # Used to manipulate vector spatial data (points, lines, polygons).


###############################################################################
# 2) Creation and verification of the base map (Italy).
###############################################################################

Italy <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Italy"
)

#Checking the map of Italy
x11() #for MacOs
italy_map<- ggplot(data = Italy) +
  geom_sf(fill = "grey95", color = "black") +
  theme_classic()

print(italy_map)
Sys.sleep(3)

###############################################################################
# 3) SPECIES 1 data collection: Tyto alba 
###############################################################################

#Define the species 1 :
myspecies1 <- "Tyto alba"

#Make a limitation of individuals to take frome GBIF:
gbif_limit <- 5000

# Set dates to limit data collection:
date_start <- as.Date("2010-01-01")
date_end   <- as.Date("2026-03-01")

# The Italy bounding box :
xmin <- 6
xmax <- 19
ymin <- 36
ymax <- 47



# Data GBIF collection for Tyto alba : 
key_tyto <- name_backbone(name = myspecies1)$usageKey

gbif_tyto_raw <- occ_data(
  taxonKey = key_tyto,
  hasCoordinate = TRUE,
  limit = gbif_limit
)

# Inspect the structure:
head(gbif_tyto_raw)
names(gbif_tyto_raw)


#Take of the species data from Italy only :
gbif_occ_tyto <- gbif_tyto_raw$data

#Filter by individuals that come frome Italy:
gbif_tyto_italy <- gbif_occ_tyto %>%
  filter(country == "Italy")


#Check of the number of individuals: 
nrow(gbif_tyto_italy)
# = 167

#Verify where the data of the species were taken in Italy : 
gbif_plot_tyto <-ggplot(data = Italy) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_tyto_italy,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  theme_classic()

print(gbif_plot_tyto)
Sys.sleep(3)




# iNaturalist data collection: 
inat_tyto_raw <- get_inat_obs(
  query = "Tyto alba",
  place_id = "italy"
)

# Inspect the structure:
head(inat_tyto_raw)
names(inat_tyto_raw)

#Take of the latitude, longitude and observations data:
inat_tyto <- data.frame(
  species = "Tyto alba",
  latitude = inat_tyto_raw$latitude,
  longitude = inat_tyto_raw$longitude,
  date_obs = as.Date(inat_tyto_raw$observed_on),
  source = "inat"
)


#Check of the number of individuals: 
nrow(inat_tyto)
# = 100 individuals


#Verify where the data for the species where taken in Italy :
inat_plot_tyto<-ggplot(data = Italy) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_tyto,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "darkred",
    color = "black"
  ) +
  theme_classic()

print(inat_plot_tyto)
Sys.sleep(3)

###############################################################################
# 4)  SPECIES 2 data collection: Athene noctua 
###############################################################################

#Define the species: 
myspecies2 <- "Athene noctua"


#Data GBIF collection for Athene noctua: 
key_athene <- name_backbone(name = myspecies2)$usageKey

#Collection of the raw data from GBIF into a variable:
gbif_athene_raw <- occ_data(
  taxonKey = key_athene,
  hasCoordinate = TRUE,
  limit = gbif_limit
)

#Inspect the structure:
head(gbif_athene_raw)
names(gbif_athene_raw)

#take of the data:
gbif_occ_athene <- gbif_athene_raw$data

#Filter by individuals that come frome Italy:
gbif_athene_italy <- gbif_occ_athene %>%
  filter(country == "Italy")

#Check of the number of individuals: 
nrow(gbif_athene_italy)
# = 56


#Verify where the data of the species were taken in Italy : 
gbif_plot_athene <- ggplot(data = Italy) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_athene_italy,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  theme_classic()

print(gbif_plot_athene)
Sys.sleep(3)



# iNaturalist data collection: 
inat_athene_raw <- get_inat_obs(
  query = "Athene noctua",
  place_id = "italy"
)
# Inspect the structure:
head(inat_athene_raw)
names(inat_athene_raw)

#Take of the latitude, longitude and observations data:
inat_athene <- data.frame(
  species = "Athene noctua",
  latitude = inat_athene_raw$latitude,
  longitude = inat_athene_raw$longitude,
  date_obs = as.Date(inat_athene_raw$observed_on),
  source = "inat"
)

#Check of the number of individuals: 
nrow(inat_athene)
# = 100

#Verify where the data for the species where taken in Italy : 
inat_plot_athene <- ggplot(data = Italy) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_athene,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "darkred",
    color = "black"
  ) +
  theme_classic()

print(inat_plot_athene)
Sys.sleep(3)



###############################################################################
# 5) Switch format GBIF data 
###############################################################################

#Make sure that Latitude and Longitude data are with the same metric when data are going to merge:
gbif_tyto <- data.frame(
  species = "Tyto alba",
  latitude = gbif_tyto_italy$decimalLatitude,
  longitude = gbif_tyto_italy$decimalLongitude,
  date_obs = as.Date(gbif_tyto_italy$eventDate),
  source = "gbif"
)

gbif_athene <- data.frame(
  species = "Athene noctua",
  latitude = gbif_athene_italy$decimalLatitude,
  longitude = gbif_athene_italy$decimalLongitude,
  date_obs = as.Date(gbif_athene_italy$eventDate),
  source = "gbif"
)

###############################################################################
# 6) Merge all data
###############################################################################

#Creation of the main matrix:
matrix_full <- bind_rows(
  gbif_tyto,
  inat_tyto,
  gbif_athene,
  inat_athene
)


###############################################################################
# 7) Clean data
###############################################################################


#Delete duplicated rows/observations:
matrix_full <- matrix_full %>%
  distinct()

#Check the new number of observations:
nrow(matrix_full)

###############################################################################
# 8) Quick check
###############################################################################

head(matrix_full)
table(matrix_full$species)
table(matrix_full$source)

###############################################################################
# 9) Creation of general observation map to verify positions of species in Italy.
###############################################################################

p1 <- ggplot() +
  geom_sf(data = Italy, fill = "grey90") +
  geom_point(
    data = matrix_full,
    aes(x = longitude, y = latitude, color = species),
    alpha = 0.6
  ) +
  theme_classic()

print(p1)
