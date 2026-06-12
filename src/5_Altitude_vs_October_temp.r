###############################################################################
# ALTITUDE VS OCTOBER TEMPERATURE - SCATTERPLOT
###############################################################################

################################################################################
# 1) Load required packages
################################################################################
library(tidyverse) # Core data manipulation.
library(ggplot2)
library(dplyr) # Select and manipulate columns.


################################################################################
# 2) Preparation of data
################################################################################

scatter_dat <- owl %>%
  filter(species %in% c("Athene noctua", "Tyto alba")) %>%
  mutate(across(
    c(elevation, tas_current_october_c),
    as.numeric
  )) %>%
  drop_na(elevation, tas_current_october_c) %>%
  dplyr::select(species, elevation, tas_current_october_c) %>%
  rename(
    Elevation = elevation,
    `October temp.` = tas_current_october_c
  )

# Species colour selection
cols <- c(
  "Athene noctua" = "#e5a454",
  "Tyto alba"     = "#9198f1"
)


################################################################################
# 3) Scatterplot 
################################################################################

oct_plot <- ggplot(scatter_dat, aes(x = `October temp.`, y = Elevation, color = species)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_color_manual(values = cols) +
  labs(
    title = "Altitude vs October temperature",
    x = "October temperature (°C)",
    y = "Elevation (m)",
    color = NULL
  ) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#d9d8d8", linewidth = 0.3),
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.background = element_rect(fill = "#fffefe", color = NA),
    panel.background = element_rect(fill = "#f4f4f4", color = NA)
  )

x11()
print(oct_plot)


# Save the plot:
ggsave(
  "Figures/Altitude_vs_October_temp.png",
  oct_plot,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "#f5f5f5"
)
