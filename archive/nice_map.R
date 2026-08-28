


library(sf)
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(rnaturalearth)
library(rnaturalearthdata)

# --- Read & prep sections (drop Water) ---
shp <- st_read("inputs/sections.shp", quiet = TRUE) |>
  st_transform(5070) |>
  filter(!grepl("water", MAP_UNIT_N, ignore.case = TRUE)) |>
  left_join(
    tibble(
      MAP_UNIT_N = list.files("reports", pattern = "\\.html$", full.names = FALSE) |>
        tools::file_path_sans_ext() |>
        (\(x) gsub("_", " ", x))() |>
        trimws(),
      path = file.path("reports", list.files("reports", pattern = "\\.html$", full.names = FALSE))
    ),
    by = "MAP_UNIT_N"
  ) |>
  mutate(
    has_report = !is.na(path),
    onclick = ifelse(
      has_report,
      paste0('window.open("./', path, '","_blank");'),
      'alert("no report available yet");'
    )
  )

# --- States for context, clipped to sections extent ---
states <- ne_states(
  country = "United States of America",
  returnclass = "sf"
) |>
  st_transform(5070)

bbox_shp <- st_bbox(shp) |> st_as_sfc()
states_clip <- st_intersection(states, bbox_shp)

# --- Plot ---
p <- ggplot() +
  geom_sf(
    data = states_clip,
    fill = "#424345",
    color = "black",
    linewidth = 0.3
  ) +
  geom_sf_interactive(
    data = shp,
    aes(
      fill = has_report,
      onclick = onclick
    ),
    color = "#f0f2f5",
    linewidth = 0.15,
    alpha = 0.25
  ) +
  scale_fill_manual(
    values = c(`TRUE` = NA, `FALSE` = NA),
    guide = "none"
  ) +
  coord_sf(crs = 5070, expand = FALSE) +
  theme_void()

# --- Render widget ---
girafe(ggobj = p, width_svg = 16, height_svg = 9) |>
  girafe_options(
    opts_zoom(min = 1, max = 8),
    opts_toolbar(position = "topright"),
    opts_hover(css = "stroke:white;stroke-width:2;"),
    opts_hover_inv(css = "fill-opacity:0.15;")
  )


