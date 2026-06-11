

library(quarto)
library(tidyverse)


data <- read.csv("inputs/sections_2025.csv")

sections <- unique(data$section_na)
sections <- head(sections, 3)   # test run

dir.create("reports_2025", showWarnings = FALSE)

reports <- sections |>
  tibble(section_na = _) |>
  mutate(
    output_file = paste0(
      stringr::str_replace_all(section_na, "[^a-zA-Z0-9]+", "_"),
      ".html"
    )
  ) |>
  purrr::pwalk(function(section_na, output_file) {
    
    # render into working directory
    quarto::quarto_render(
      input = "demo_report_2025.qmd",
      execute_params = list(section_na = section_na),
      output_file = output_file
    )
    
    # move to reports/ after render
    file.rename(
      from = output_file,
      to   = file.path("reports_2025", output_file)
    )
  })

