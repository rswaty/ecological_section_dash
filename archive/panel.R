


# interactive panel chart: top 3 bps by area
# dataset: bps_evt

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(plotly)

# normalize evt_phys
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

# top 3 bps by area
top_bps <- bps_evt_norm %>%
  group_by(bps_name) %>%
  summarise(bps_total = sum(count), .groups = "drop") %>%
  arrange(desc(bps_total)) %>%
  slice_head(n = 3) %>%
  mutate(
    bps_pct_total = bps_total / sum(bps_total),
    bps_label = paste0(
      bps_name, " (", percent(bps_pct_total, accuracy = 1), ")"
    )
  )

# panel data
d_panel <- bps_evt_norm %>%
  semi_join(top_bps, by = "bps_name") %>%
  left_join(
    top_bps %>% select(bps_name, bps_label, bps_total),
    by = "bps_name"
  ) %>%
  group_by(bps_label, bps_total, evt_phys) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  group_by(bps_label, bps_total) %>%
  mutate(pct = count / sum(count)) %>%
  ungroup() %>%
  mutate(
    bps_label = fct_reorder(
      str_wrap(bps_label, width = 60),
      bps_total,
      .desc = TRUE
    ),
    hover_txt = paste0(
      "<b>", evt_phys, "</b><br>",
      percent(pct, accuracy = 1)
    )
  )

# muted palette you provided
evt_colors <- c(
  "Conifer"      = "#0b81a2",
  "Hardwood"     = "#36b700",
  "Shrubland"    = "#7f4e79",
  "Grassland"    = "#f0c571",
  "Riparian"     = "#59a89c",
  "Exotics"      = "#e25759",
  "Agricultural" = "#f0c571",
  "Developed"    = "#c8c8c8"
)

p <- ggplot(
  d_panel,
  aes(
    x = bps_label,
    y = pct,
    fill = evt_phys,
    text = hover_txt
  )
) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = evt_colors, drop = TRUE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "existing vegetation composition within dominant historical bps",
    subtitle = "bars ordered by historical area; labels show percent of total (top 3)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"
  )

ggplotly(p, tooltip = "text")
