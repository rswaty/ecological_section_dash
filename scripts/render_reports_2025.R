

library(quarto)
library(tidyverse)


data <- read.csv("inputs/sections_2025.csv")

sections <- unique(data$section_na)
# to render some
sections <- sections[86:length(sections)]


dir.create("reports", showWarnings = FALSE)

reports <- sections |>
  tibble(section_na = _) |>
  mutate(
    output_file = paste0(
      stringr::str_replace_all(section_na, "[^a-zA-Z0-9]+", "_"),
      ".html"
    )
  ) |>
  purrr::pwalk(function(section_na, output_file) {
    
    message("Rendering: ", section_na)
    gc()
    
    quarto::quarto_render(
      input = "demo_report.qmd",
      execute_params = list(section_na = section_na),
      output_file = output_file
    )
    
    file.rename(
      from = output_file,
      to = file.path("reports", output_file)
    )
    
  })



