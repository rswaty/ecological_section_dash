

library(sf)
library(dplyr)
library(ggplot2)
library(rmapshaper)
library(rnaturalearth)

# output folder
out_dir <- file.path("locator_maps_2025")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# filename helper: deterministic + filesystem safe
slugify <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

# read and project ecological sections
sections <- st_read("inputs/sections_2025.shp", quiet = TRUE) %>%
  st_transform(5070)

# simplify for lightweight rendering
sections_simp <- ms_simplify(
  sections,
  keep = 0.05,
  keep_shapes = TRUE
)

# state boundaries (lines only)
states <- ne_states(
  country = "united states of america",
  returnclass = "sf"
) %>%
  st_transform(5070) %>%
  st_geometry()

# lakes (optional but you already pull them; helps Great Lakes orientation)
lakes <- ne_download(
  scale = 50,
  type = "lakes",
  category = "physical",
  returnclass = "sf"
) %>%
  st_transform(5070) %>%
  ms_simplify(keep = 0.03, keep_shapes = TRUE)

# fixed US-wide extent (your approach)
bb <- st_bbox(sections_simp)

# function that returns your locator map for one selected section
make_locator_plot <- function(section_sel) {
  ggplot() +
    geom_sf(
      data = lakes,
      fill = "#d6e9f5",
      color = NA
    ) +
    geom_sf(
      data = sections_simp,
      fill = "grey95",
      color = NA
    ) +
    geom_sf(
      data = states,
      color = "grey75",
      linewidth = 0.25
    ) +
    geom_sf(
      data = section_sel,
      fill = "#5e8568",
      color = "#4d4d4d",
      linewidth = 0.7
    ) +
    coord_sf(
      xlim = bb[c("xmin", "xmax")],
      ylim = bb[c("ymin", "ymax")],
      datum = NA,
      expand = FALSE
    ) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.margin = margin(6, 6, 14, 6)
    )
}

# generate one jpg per MAP_UNIT_N
map_names <- sort(unique(sections_simp$SECTION_NA))

# optional lookup table for debugging / joining later
lookup <- data.frame(
  SECTION_NA = map_names,
  slug = slugify(map_names),
  file = paste0(slugify(map_names), ".jpg"),
  stringsAsFactors = FALSE
)

for (nm in map_names) {
  section_sel <- sections_simp %>%
    filter(SECTION_NA == nm)
  
  # if there are unexpected duplicates, skip safely
  if (nrow(section_sel) != 1) next
  
  outfile <- file.path(out_dir, paste0(slugify(nm), ".jpg"))
  
  p <- make_locator_plot(section_sel)
  
  # ggsave is the standard way to save ggplots; JPEG tends to be smaller than PNG for maps [2](https://ggplot2.tidyverse.org/reference/ggsave.html)
  ggsave(
    filename = outfile,
    plot = p,
    device = "jpeg",
    width = 10.5,
    height = 4.2,
    units = "in",
    dpi = 150,
    quality = 85
  )
}

write.csv(lookup, file.path(out_dir, "locator_map_lookup.csv"), row.names = FALSE)
