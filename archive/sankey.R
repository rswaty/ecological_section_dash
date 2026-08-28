

library(dplyr)
library(plotly)
library(scales)
library(stringr)

# muted palette (as provided)
evt_colors <- c(
  "Conifer"      = "#4b7054",
  "Hardwood"     = "#70fa91",
  "Shrubland"    = "#7f4e79",
  "Grassland"    = "#f0c571",
  "Riparian"     = "#0b81a2",
  "Exotics"      = "#e25759",
  "Agricultural" = "#f0c571",
  "Developed"    = "#c8c8c8"
)

# color bps nodes by groupveg (fallback to gray)
groupveg_colors <- c(
  "Conifer"   = "#4b7054",
  "Hardwood"  = "#70fa91",
  "Shrubland" = "#7f4e79",
  "Grassland" = "#f0c571",
  "Riparian"  = "#0b81a2",

)

# normalize evt_phys once (and remove sparsely vegetated)
bps_evt_norm <- bps_evt %>%
  mutate(
    evt_phys = case_when(
      evt_phys %in% c("Exotic Herbaceous", "Exotic Tree-Shrub") ~ "Exotics",
      evt_phys == "Agricultural"                               ~ "Agricultural",
      evt_phys == "Developed"                                  ~ "Developed",
      grepl("^Developed", evt_phys)                            ~ "Developed",
      grepl("Quarries|Strip Mines|Gravel", evt_phys)           ~ "Developed",
      evt_phys == "Sparsely Vegetated"                         ~ NA_character_,
      TRUE ~ evt_phys
    )
  ) %>%
  filter(!is.na(evt_phys))

# top 3 bps by area, keep groupveg + percent of total
top_bps <- bps_evt_norm %>%
  group_by(bps_name, groupveg) %>%
  summarise(bps_total = sum(count), .groups = "drop") %>%
  mutate(bps_pct_total = bps_total / sum(bps_total)) %>%
  arrange(desc(bps_total)) %>%
  slice_head(n = 3) %>%
  mutate(
    bps_label = paste0(
      str_wrap(bps_name, width = 55), " (", percent(bps_pct_total, accuracy = 1), ")"
    ),
    bps_color = if_else(groupveg %in% names(groupveg_colors),
                        groupveg_colors[groupveg],
                        "#c8c8c8")
  )

make_sankey <- function(bps, bps_label, bps_color) {
  
  flows <- bps_evt_norm %>%
    filter(bps_name == bps) %>%
    group_by(evt_phys) %>%
    summarise(count = sum(count), .groups = "drop") %>%
    mutate(pct = count / sum(count)) %>%
    arrange(desc(pct))
  
  nodes <- c(bps_label, flows$evt_phys)
  
  node_index <- tibble(
    name = nodes,
    id = seq_along(nodes) - 1
  )
  
  links <- flows %>%
    left_join(node_index, by = c("evt_phys" = "name")) %>%
    mutate(
      source = 0,
      target = id,
      value  = pct,
      label  = paste0(evt_phys, ": ", percent(pct, accuracy = 1))
    )
  
  node_colors <- c(
    bps_color,
    ifelse(flows$evt_phys %in% names(evt_colors), evt_colors[flows$evt_phys], "#c8c8c8")
  )
  
  plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = nodes,
      color = node_colors,
      pad = 18,
      thickness = 20
    ),
    link = list(
      source = links$source,
      target = links$target,
      value  = links$value,
      label  = links$label
    )
  )
}

# three separate sankeys for the top 3 bps
lapply(seq_len(nrow(top_bps)), function(i) {
  make_sankey(
    bps       = top_bps$bps_name[i],
    bps_label = top_bps$bps_label[i],
    bps_color = top_bps$bps_color[i]
  )
})
