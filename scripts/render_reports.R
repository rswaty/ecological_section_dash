

library(quarto)
library(tidyverse)


data <- read.csv("inputs/sections.csv")

sections <- unique(data$map_unit_n)
sections <- head(sections, 3)   # test run

dir.create("reports", showWarnings = FALSE)

reports <- sections |>
  tibble(map_unit_n = _) |>
  mutate(
    output_file = paste0(
      stringr::str_replace_all(map_unit_n, "[^a-zA-Z0-9]+", "_"),
      ".html"
    )
  ) |>
  purrr::pwalk(function(map_unit_n, output_file) {
    
    # render into working directory
    quarto::quarto_render(
      input = "demo_report.qmd",
      execute_params = list(map_unit_n = map_unit_n),
      output_file = output_file
    )
    
    # move to reports/ after render
    file.rename(
      from = output_file,
      to   = file.path("reports", output_file)
    )
  })
