

library(tidyverse)

data <- read_csv("inputs/combine_raw.csv")



slugify <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

sections <- unique(data$MAP_UNIT_N)

dir.create("reports", showWarnings = FALSE)

purrr::walk(
  sections,
  function(sec) {
    
    out_file <- paste0(slugify(sec), "-report.html")
    
    cmd <- sprintf(
      'quarto render demo_report.qmd --no-project -P map_unit_n="%s" --output %s --output-dir reports',
      sec,
      out_file
    )
    
    system(cmd)
  }
)
