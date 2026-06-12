###############################################################################
#INTERMEDIATE PROJECT - BIODIVERSITY DATA ANALYSIS
###############################################################################

#Script of the step 1 : Combining GBIF + iNatursalist occurences of the 2 species,
#Tyto alba and Athene noctua, and creation of the main matrix.
source("./src/Import_gbif_inat.R")


#Script of the step 2 : Adding ecosystem data to species occurence coordinates.
source("./src/Import_ecosystem.r")


#Script of the step 3 : Adding elevation data to species occurence coordinates and to the main matrix. 
source("./src/Import_elevation.r")


#Script of the step 4 : Adding climat data to species occurence coordinates and to the main matrix. 
source("./src/Import_climat.R")


#Script of the step 5 : Adding satellite (NDVI) data to species occurence coordinates and to the main matrix. 
source("./src/Import_sat_manual.r")
#Finally, the completed matrix contains 388 occurrences of the two species. 
#(Only the column containing the data collection dates has missing values (NA); the other columns are complete.)


###############################################################################
#### FINAL PROJECT ####
###############################################################################

# Main question : 
#Do barn owls and little owls occupy the same ecological niches in Italy?


# 1 Load the final environmental matrix
source("./src/1_Read_matrix.R")


# 2 PCA - Biplot
source("./src/2_PCA_Biplot.r")
# Interpretations : The PCA reveals considerable overlap between the ecological niches of 
#Tyto alba and Athene noctua, which seems to suggest that the two species generally occupy 
#a similar environmental space. However, the ordination also shows partial differentiation 
#along the main ecological gradients. The first component, which explains the largest 
#proportion of the variance, primarily contrasts warmer sites with those situated at higher 
#altitudes, which are more humid and have greater vegetation cover. So Tyto alba appears to 
#be more associated with the warmer side of this gradient, whilst Athene noctua is more 
#commonly found in sites characterised by higher altitude, higher annual rainfall and higher 
#NDVI values, i.e. with greater vegetation density. The second component also reflects a 
#gradient linked to the warming trend in October (the period of highest activity for both 
#species) and other thermal variables, although the overlap between the two species remains 
#marked. 


# 3 Random Forest model and discriminant variable analysis, with a feature importance plot.
source("./src/3_Random_forest_&_discriminating_variable.r")
#Interpretation: The variable importance plot shows that the distinction between the two species is 
#primarily based on large-scale environmental gradients, particularly altitude and annual 
#precipitation, followed by thermal and land-use variables. These results suggest that the 
#two species occupy partially differentiated ecological niches, whilst categorical variables
# such as "Moisture" or "Temperature" have a very minor role in this model.


# 4 Environmental variable comparison between species - ridgeline plot and boxplot.
source("./src/4_Environmental_variable_comparison.r")
# Interpretation: The figure highlights a partial ecological differentiation between 
#Athene noctua and Tyto alba. The separation of the species is primarily driven by altitude
# and climatic variables, with A. noctua associated with relatively higher, wetter and 
#cooler conditions, whilst T. alba is more closely linked to lower-lying and warmer 
#environments. The NDVI values show a weaker signal, indicating and confirming that 
#topographic and climatic gradients have a greater influence than plant productivity 
#in explaining the differences observed between species.


# 5 Altitude vs october temperature - scatterplot.
source("./src/5_Altitude_vs_October_temp.r")
# Interpretation: The relationship between altitude and temperature in October 
#suggests a partial segregation of ecological niches between these two owl species, 
#with Athene noctua tending to occupy higher and cooler sites than Tyto alba, which is 
#consistent with and reinforces a more pronounced differentiation along the 
#temperature-altitude gradient.

# 6 Interactive map figure.
source("./src/6_Interactive_map.r")


# 7 Final summary panel figure.
source("./src/7_Summary_figure.r")

# General interpretation: These two species appear to share a wide common environmental
# range in Italy, but their ranges don't overlap completely overall. This therefore 
#indicates a relatively close and overlapping sharing of niches and tolerance gradients 
#rather than a complete equivalence of these, being primarily determined by altitude and 
#average temperatures in October (a period of high activity for both species), with 
#additional contributions linked to precipitation and vegetation conditions.