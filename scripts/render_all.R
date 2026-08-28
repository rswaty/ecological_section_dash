
library(furrr)

plan(multisession, workers = 4)

future_pwalk(
  list(section_na = reports$section_na,
       output_file = reports$output_file),
  function(section_na, output_file) {
    
    quarto::quarto_render(
      input = "demo_report.qmd",
      execute_params = list(section_na = section_na),
      output_file = output_file
    )
    
    file.rename(
      output_file,
      file.path("reports", output_file)
    )
  }
)
