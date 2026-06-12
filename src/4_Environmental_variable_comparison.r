################################################################################
# COMPARE VARIABLES BETWEEN THE TWO OWL SPECIES - RIDGELINE PLOT AND BOXPLOT
################################################################################


################################################################################
# 1) Load required packages
################################################################################
library(tidyverse)  # Core data manipulation.
library(ggplot2)
library(ggridges)  # Create Ridgeline plots.
library(patchwork)  # Combine multiple ggplots into one figure layout.
library(ggtext)   # Add formatted text with Markdown/HTML in ggplot.
library(dplyr)  # Data filtering, mutation, select, joins, and summaries.


# ################################################################################
# 2) Prepare the variables.
# ################################################################################

#Keep both species and the environmental variables to compare:
dat <- owl %>%
  filter(species %in% c("Athene noctua", "Tyto alba")) %>%
  mutate(across(
    c(elevation, tmax_mean_c, prec_annual, tas_current_october_c, NDVI),
    as.numeric
  )) %>%
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
  )

# Define the colours associated with each species:
cols <- c(
  "Athene noctua" = "#e5a454",
  "Tyto alba" = "#9198f1"
)

# ################################################################################
# 3) Convert the data to long format and standardise each variable so that they 
#    can be compared on the same scale.
# ################################################################################

long_dat <- dat %>%
  pivot_longer(
    cols = -species,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(z_value = as.numeric(scale(value))) %>%
  ungroup()


# Set the order of the variables in the figure:
var_order <- c("Elevation", "Annual precip.","October temp.", "Max temp.", "NDVI")

# Convert the variable to a factor to impose the display order:
long_dat <- long_dat %>%
  mutate(variable = factor(variable, levels = rev(var_order)))


# Preparing the data for boxplots:
box_dat <- long_dat %>%
  dplyr::select(species, variable, z_value)

box_dat <- box_dat %>%
  mutate(variable = factor(variable, levels = c(
    "Elevation",
    "Max temp.",
    "Annual precip.",
    "October temp.",
    "NDVI"
  )))


# ################################################################################
# 4) The Ridgeline plot
# ################################################################################

ridge_plot <- ggplot(long_dat, aes(x = z_value, y = variable, fill = species)) +
  geom_density_ridges(
    color = "white",
    alpha = 0.8,
    scale = 1.0,
    rel_min_height = 0.01,
    linewidth = 0.25
  ) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(limits = c(-3.2, 3.2), expand = c(0, 0)) +
  labs(
    title = "A. Probability density of standardized gradients",
    x = "Standardized value (z-score)",
    y = NULL
  ) +
  theme_minimal(base_size = 13, base_family = "serif") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.35),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 13),
    plot.background = element_rect(fill = "grey95", color = NA),
    panel.background = element_rect(fill = "grey95", color = NA)
  )

# ################################################################################
# 5) The Boxplot 
# ################################################################################
box_plot <- ggplot(box_dat, aes(x = variable, y = z_value, fill = species)) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.75),
    width = 0.58,
    alpha = 0.85,
    linewidth = 0.3,
    color = "#131313"
  ) +
  geom_jitter(
    position = position_jitterdodge(
      dodge.width = 0.75,
      jitter.width = 0.12
    ),
    size = 0.7,
    color = "#1f1f1f",
    alpha = 0.5,
    shape = 16
  ) +
  scale_fill_manual(values = cols) +
  labs(
    title = "B. Inter-specific variation in standardized predictor values",
    x = NULL,
    y = "Standardized value (z-score)"
  ) +
  theme_minimal(base_size = 13, base_family = "serif") +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 12),
    plot.background = element_rect(fill = "grey95", color = NA),
    panel.background = element_rect(fill = "grey95", color = NA)
  )

# ################################################################################
# 6) Vertical assembly
# ################################################################################

x11()
final_plot <- ridge_plot / box_plot +
  plot_layout(heights = c(1.25, 1)) +
  plot_annotation(
    title = "Environmental niche differenciation of Athene noctua and Tyto alba",
    subtitle = "Upper panel: Standardized distributions of key environmental variables; lower panel: species-specific value ranges",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, family = "serif"),
      plot.subtitle = element_text(size = 10, hjust = 0.5, family = "serif"),
      plot.background = element_rect(fill = "grey95", color = NA)
    )
  ) &
  theme(
    plot.background = element_rect(fill = "grey95", color = NA)
  )

print(final_plot)


# Save the plot:
ggsave(
  "Figures/Owl_ridge_box_vertical.png",
  final_plot,
  width = 11.5,
  height = 10,
  dpi = 300,
  bg = "grey95"
)
