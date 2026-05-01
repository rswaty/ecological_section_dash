

library(sf)
library(dplyr)
library(ggplot2)
library(rmapshaper)
library(rnaturalearth)

# read and project sections
sections <- st_read("inputs/sections.shp", quiet = TRUE) %>%
  st_transform(5070)

# simplify sections
sections_simp <- ms_simplify(
  sections,
  keep = 0.05,
  keep_shapes = TRUE
)

# focal section
section_name <- "Okanogan Highland"

section_sel <- sections_simp %>%
  filter(MAP_UNIT_N == section_name)

stopifnot(nrow(section_sel) == 1)

# states (geometry-only, very light)
states <- ne_states(
  country = "united states of america",
  returnclass = "sf"
) %>%
  st_transform(5070) %>%
  ms_simplify(keep = 0.02, keep_shapes = TRUE) %>%
  st_geometry()

# lakes
lakes <- ne_download(
  scale = 50,
  type = "lakes",
  category = "physical",
  returnclass = "sf"
) %>%
  st_transform(5070) %>%
  ms_simplify(keep = 0.03, keep_shapes = TRUE)

# zoomed extent with generous padding
bb <- st_bbox(section_sel)
pad_x <- (bb["xmax"] - bb["xmin"]) * 2.0
pad_y <- (bb["ymax"] - bb["ymin"]) * 2.0

xlim <- c(bb["xmin"] - pad_x, bb["xmax"] + pad_x)
ylim <- c(bb["ymin"] - pad_y, bb["ymax"] + pad_y)

# locator map
ggplot() +
  geom_sf(
    data = lakes,
    fill = "#d0e6f2",
    color = NA
  ) +
  geom_sf(
    data = sections_simp,
    fill = "grey94",
    color = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = states,
    color = "grey70",
    linewidth = 0.40
  ) +
  geom_sf(
    data = section_sel,
    fill = "#3690c0",
    color = "black",
    linewidth = 1.0
  ) +
  coord_sf(
    xlim = xlim,
    ylim = ylim,
    datum = NA,
    expand = FALSE
  ) +
  labs(title = section_name) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0
    ),
    plot.margin = margin(5, 5, 20, 5)
  )
