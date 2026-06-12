###############################################################################
# ADDING CLIMAT DATA TO SPECIES OCCURRENCE COORDINATES
###############################################################################


################################################################################
# 1) Load required packages
################################################################################

library(Rchelsa) # Allows downloading and using CHELSA climate data for ecological and environmental analyses.
library(terra) # Used to manipulate, analyze, and extract spatial raster and vector data.
library(dplyr) # Facilitates the manipulation, filtering, and organization of dataframes.
library(ggplot2)  # Used to create graphs and visualizations of data.

################################################################################
# 2) Check the input data.
################################################################################
# matrix_full_eco_elev contain :
# species, longitude, latitude variables.

str(matrix_full_eco_elev)
# Yes.

################################################################################
# 3) Filter the 2 species.
################################################################################

#Filter:
species_df <- matrix_full_eco_elev %>%
  filter(species %in% c("Tyto alba", "Athene noctua")) %>%
  mutate(occurrence_id = row_number())

# Plot to visualize :
x11()
species_df_p <- ggplot() +
  geom_sf(
    data = Italy,
    fill = "grey95",
    color = "black"
  ) +
  geom_point(
    data = species_df,
    aes(
      x = longitude,
      y = latitude,
      color = species
    ),
    alpha = 0.7
  ) +theme_classic() +
  labs(
    title = "Occurrences of both species"
  )

print(species_df_p)
Sys.sleep(3)

################################################################################
# 4) Extract the coordinates.
################################################################################

coords_unique <- species_df %>%
  distinct(longitude, latitude)

################################################################################
# 5) Extract the temperature max (2021)
################################################################################
#Extraction of Temperature variable by getChelsea() command:
#(Data from 2021 to have the most available recent data)
tmax_r <- getChelsa(
  var       = "tasmax",
  coords    = coords_unique,
  startdate = as.Date("2021-01-01"),
  enddate   = as.Date("2021-12-31"),
  dataset   = "chelsa-monthly"
)

# Matrix conversion :
tmax_mat <- tmax_r %>%
  select(-time) %>%
  as.matrix()


# Annual average :
tmax_df <- coords_unique %>%
  mutate(
    tmax_mean_c = colMeans(tmax_mat, na.rm = TRUE) - 273.15
  )

# Merge with longitude and latitude locations from species :
species_df <- species_df %>%
  left_join(tmax_df, by = c("longitude", "latitude"))

# Visualize difference of temparature max gradient for each species :
t_max_p <- ggplot(species_df, aes(x = tmax_mean_c, fill = species)) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  labs(title = "Distribution Tmax 2020")

print(t_max_p)
Sys.sleep(3)

################################################################################
# 6) Extract precipitation (2021)
################################################################################
#Extraction of precipitation variable by getChelsea() command:
#(Data from 2021 to have the most available recent data)
prec_r <- getChelsa(
  var       = "pr",
  coords    = coords_unique,
  startdate = as.Date("2021-01-01"),
  enddate   = as.Date("2021-12-31"),
  dataset   = "chelsa-monthly"
)

# Matrix conversion :
prec_mat <- prec_r %>%
  select(-time) %>%
  as.matrix()

# Annual average :
prec_df <- coords_unique %>%
  mutate(
    prec_annual = colSums(prec_mat, na.rm = TRUE) 
  )  # somme of precipitation

# Merge with longitude and latitude locations from species :
species_df <- species_df %>%
  left_join(prec_df, by = c("longitude", "latitude"))


# Visualize difference of precipitation gradient for each species :
prec_p <- ggplot(species_df, aes(x = prec_annual, fill = species)) +
  geom_histogram(bins = 30, alpha = 0.6) +
  theme_classic() +
  labs(title = "Annual precipitation 2021")

print(prec_p)
Sys.sleep(3)

################################################################################
# 7) Creation of temperature forecasts for the month of october for the 2 species.
#(Owls are the most active in the October month during the year)
################################################################################
# 7.1) Begin with current October temperature (fixed) over 1981-2010.
################################################################################
#Extraction of temperature variable over 1981-2010 by getChelsea() command:
tas_cur_october <- getChelsa(
  var     = "tas",
  coords  = coords_unique,
  date    = c(10, 1981, 2010),
  dataset = "chelsa-climatologies"
)

# Matrix conversion :
tas_mat <- tas_cur_october %>%
  select(-time) %>%
  as.matrix()

# Average over the 3 periods:
tas_mean <- colMeans(tas_mat, na.rm = TRUE)

# Switch from the metric Kelvin to Celsius :
tas_cur_october_df <- coords_unique %>%
  mutate(
    tas_current_october_c = tas_mean - 273.15
  )

# Merge with longitude and latitude locations from species :
species_df <- species_df %>%
  left_join(tas_cur_october_df, by = c("longitude", "latitude"))
  

# Visualize difference of recent temperature gradient of october for each species :
current_t_p <- ggplot(species_df, aes(x = tas_current_october_c, fill = species)) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  labs(title = "Temperature gradient in October for owl species")

print(current_t_p)
Sys.sleep(3)

################################################################################
# 7.2) Future climate conditions in October : temperature in 2050 under SSP126
################################################################################
#Extraction of temperature variable in October 2050 by getChelsea() command:
tas_fut_october <- getChelsa(
  var     = "tas",
  coords  = coords_unique,
  date    = as.Date("2050-10-01"),
  dataset = "chelsa-climatologies",
  ssp     = "ssp126",
  forcing = "MPI-ESM1-2-HR"
)

# Switch from the metric Kelvin to Celsius :
tas_fut_october_df <- coords_unique %>%
  mutate(
    tas_future_october_2050_c =
      as.numeric(tas_fut_october %>% select(-time) %>% unlist()) - 273.15
  )

# Merge with longitude and latitude locations from species :
species_df <- species_df %>%
  left_join(tas_fut_october_df, by = c("longitude", "latitude"))


# Visualize difference of futur temperature gradient of october for each species :
futur_t_p <- ggplot(species_df, aes(x = tas_future_october_2050_c, fill = species)) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  labs(title = "Temperature gradient in October 2050 for owl species")

print(futur_t_p)
Sys.sleep(3)

################################################################################
# 8) Merge all climat data to the main matrix.
################################################################################

#Merging of matrix of climat variables to the main matrix:
matrix_full_eco_elev_climat <- species_df %>%
  mutate(
    delta_tas_october_c =
      tas_future_october_2050_c - tas_current_october_c
  )


#Visulization of differences beetween current and futur temperature gradients for both species:
p4 <- ggplot(matrix_full_eco_elev_climat,
       aes(x = tas_current_october_c,
           y = tas_future_october_2050_c,
           color = species)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "Current vs future temperatures")

#Display the plot :
print(p4)
