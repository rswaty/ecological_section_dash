


library(tidyverse)

forests <- unique(data$forestname)


# try to outputs
purrr::map(
  .x = forests,  # vector of param values
  .f = ~rmarkdown::render(
    input = "demo_report.qmd",  # RMarkdown filepath
    params = list(forestname = .x),  # iterated parameter value
    output_file = stringr::str_glue("output/", .x, "-report.html")  # iterated output path
  )
)
