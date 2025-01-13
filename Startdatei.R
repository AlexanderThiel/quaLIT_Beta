options(shiny.autoreload = TRUE)
options(shiny.launch.browser = TRUE)

required_packages <- c("shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets",
                       "pdftools", "readr", "rmarkdown", "stringr", "pagedown", 
                       "jsonlite", "fresh", "visNetwork","webshot", "shinyjs",
                       "plotly", "networkD3", "sunburstR", "tidyr", "htmlwidgets",
                       "dplyr", "ggplot2", "reshape2", "scales", "RColorBrewer", "webshot2")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}
lapply(required_packages, require, character.only = TRUE)


script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
if (is.null(script_dir) || script_dir == "") {
  stop("Could not determine script directory. Please ensure you're running this from RStudio.")
}

dir.create(file.path(script_dir, "Qual_Funktionen"), showWarnings = FALSE)
dir.create(file.path(script_dir, "Reden"), showWarnings = FALSE)
dir.create(file.path(script_dir, "Reden/nb"), showWarnings = FALSE)
dir.create(file.path(script_dir, "Reden/nb/codings"), showWarnings = FALSE)
dir.create(file.path(script_dir, "Reden/nb/graphs"), showWarnings = FALSE)
dir.create(file.path(script_dir, "Reden", "nb", "analytics"), showWarnings = FALSE)

config_path <- file.path(script_dir, "categories.json")
categories <- fromJSON(config_path)$categories

source(file.path(script_dir, "Qual_Funktionen", "text_config.R"))
source(file.path(script_dir, "Qual_Funktionen", "calculate_reliability.R"))

# Vorverarbeitung der PDFs
input_pdfs <- list.files(file.path(script_dir, "Reden"), pattern = "\\.pdf$", full.names = TRUE)
for (input_pdf in input_pdfs) {
  input_name <- tools::file_path_sans_ext(basename(input_pdf))
  output_pdf <- file.path(script_dir, "Reden/nb", paste0(input_name, "_nb.pdf"))
  if (!file.exists(output_pdf)) {
    result <- remove_bracketed_text(
      input_path = input_pdf,
      output_path = output_pdf,
      bracket_types = c("round", "square")
    )
    cat("Processed: ", input_pdf, "\n")
  } else {
    cat("Skipped (already converted): ", input_pdf, "\n")
  }
}

directory <- file.path(script_dir, "Reden", "nb")
aggregated_texts <- aggregate_texts(directory)

# Start quaLIT
source(file.path(script_dir, "Qual_Funktionen", "quaLIT_interface.R"))
run_quaLIT(aggregated_texts, categories, script_dir)