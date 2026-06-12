###############################################################################
# FIRST ANALISYS - PCA / BIPLOT
###############################################################################

################################################################################
# 1) Load required packages
################################################################################
library(tidyverse) # Core data manipulation.
library(dplyr)    # Data filtering, mutation, select, joins, and summaries.
library(factoextra) # PCA visualisation tools, including biplots.


################################################################################
# 2) Check and preparation of the necessary variables.
################################################################################
# Check the columns:
names(owl)

# Selection of environmental variables:
vars <- owl %>%
  dplyr::select(
    elevation,
    tmax_mean_c,
    prec_annual,
    tas_current_october_c,
    tas_future_october_2050_c,
    delta_tas_october_c,
    NDVI
  )

# Convert to numeric format:
vars <- vars %>%
  mutate(across(everything(), as.numeric))


# Adjustment of variable names:
  vars <- owl %>%
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

# Remove rows containing NA values:
complete_rows <- complete.cases(vars)

vars_clean <- vars[complete_rows, ]

# Keep the corresponding species:
owl_clean <- owl[complete_rows, ]

# Remove constant variables:
vars_clean <- vars_clean[, apply(vars_clean, 2, sd, na.rm = TRUE) > 0]

# Verification:
print(sapply(vars_clean, sd))

# PCA
pca <- prcomp(
  vars_clean,
  center = TRUE,
  scale. = TRUE
)

# Explained variance: 
summary(pca)


# Variable contributions :
x11()
dim1<-fviz_contrib(
  pca,
  choice = "var",
  axes = 1,
  top = 10
)
print(dim1)
Sys.sleep(3)

dim2 <- fviz_contrib(
  pca,
  choice = "var",
  axes = 2,
  top = 10
)
print(dim2)
Sys.sleep(3)

# Scree plot
all_dim <- fviz_eig(
  pca,
  addlabels = TRUE
)
print(all_dim)
Sys.sleep(3)


# Biplot:
biplot <- fviz_pca_biplot(
  pca,
  geom.ind = "point",
  habillage = as.factor(owl_clean$species),
  addEllipses = TRUE,
  ellipse.level = 0.95,
  pointsize = 2,
  label = "var",
  repel = TRUE,
  col.var = "black",
  palette = c("#e5a454", "#9198f1"),
   select.var = list(contrib = 8) 
) +
  theme_minimal(base_size = 15, base_family = "serif") + 
  theme(
    panel.background = element_rect(fill = "#f3f3f3", color = NA),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.grid = element_blank(),
    legend.position = "top",
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

print(biplot)


# Save the plot:
dir.create("Figures")
ggsave(
  filename = "Figures/Owl_PCA_Biplot.png",
  plot = biplot,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)

