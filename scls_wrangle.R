

# 1) current observed data
current_data <- data |>
  select(count, bps_model, bps_name, label) |>
  unite(model_label, c("bps_model", "label"), remove = FALSE) |>
  group_by(bps_model, model_label, label, bps_name) |>
  summarise(count = sum(count), .groups = "drop") |>
  group_by(bps_model) |>
  mutate(
    bps_total = sum(count),
    current_percent = round(count / bps_total * 100)
  ) |>
  ungroup()

# get list of bps_model values for aoi

top_bps_models <- current_data |>
  select(bps_model, bps_name) |>
  filter(bps_name %in% top_bps) |>
  group_by(bps_name, bps_model) 


#2) aoi ref con

aoi_ref_con <- ref_con |>
  filter(bps_model %in% top_bps_models$bps_model) |>
  select(model_label, ref_percent)

#3) scls ref and current

scls_data <- aoi_ref_con |>
  left_join(current_data, by = "model_label")


