#text_config.R

remove_bracketed_text <- function(input_path, output_path, bracket_types = c("round",
                                                                             "square", "curly")) {
  if (!file.exists(input_path)) {
    stop("Input file does not exist: ", input_path)
  }
  required_packages <- c("pdftools", "stringr", "pagedown")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace,
                                                quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Required packages missing: ", paste(missing_packages, collapse = ", "))
  }
  bracket_patterns <- list(
    round = "\\([^\\)]*\\)",
    square = "\\[[^\\]]*\\]",
    curly = "\\{[^\\}]*\\}"
  )
  tryCatch({
    Sys.setlocale("LC_ALL", "German")
    pdf_text <- pdftools::pdf_text(input_path)
    processed_text <- sapply(pdf_text, function(page) {
      page <- iconv(page, "", "UTF-8", sub = "")
      for (type in bracket_types) {
        page <- stringr::str_replace_all(page, bracket_patterns[[type]], "")
      }
      page <- stringr::str_replace_all(page, "\\s+", " ")
      return(page)
    })
    temp_html <- tempfile(fileext = ".html")
    writeLines(
      c('<!DOCTYPE html>',
        '<html>',
        '<head>',
        '<meta charset="UTF-8">',
        '</head>',
        '<body>',
        processed_text,
        '</body>',
        '</html>'
      ),
      temp_html,
      useBytes = TRUE
    )
    pagedown::chrome_print(
      temp_html,
      output = output_path,
      options = list(
        preferCSSPageSize = TRUE,
        printBackground = TRUE
      )
    )
    
    unlink(temp_html)
    return(TRUE)
  }, error = function(e) {
    warning("Error processing PDF: ", e$message)
    return(FALSE)
  })
}

extract_text <- function(file_path) {
  ext <- tools::file_ext(file_path)
  if (ext == "pdf") {
    text <- pdftools::pdf_text(file_path)
    text <- paste(text, collapse = "\n")
  } else if (ext == "txt") {
    text <- read_lines(file_path)
  } else if (ext == "md") {
    temp_html <- tempfile(fileext = ".html")
    rmarkdown::render(file_path, output_file = temp_html)
    text <- readLines(temp_html)
  } else {
    stop("Unsupported file type")
  }
  return(text)
}

split_sentences <- function(text) {
  text <- gsub("\n", " ", text)
  text <- gsub("\\.{2,}", " MULTIPLEDOTS ", text)
  text <- gsub(":", " COLON ", text)
  sentences <- unlist(strsplit(text, "(?<!\\.)\\.(?!\\.)", perl = TRUE))
  sentences <- trimws(sentences)
  sentences <- gsub(" MULTIPLEDOTS ", "..", sentences)
  sentences <- gsub(" COLON ", ":", sentences)
  sentences <- sentences[sentences != ""]
  return(sentences)
}

aggregate_texts <- function(directory) {
  files <- list.files(directory, full.names = TRUE, pattern = "\\.pdf$")
  data <- data.frame(party = character(), year = integer(), text = character(), stringsAsFactors = FALSE)
  
  for (file in files) {
    file_name <- basename(file)
    cat("Processing file:", file_name, "\n")
    text <- extract_text(file)
    sentences <- split_sentences(text)
    
    info <- str_match(file_name, "^([A-Za-z]+) ([A-Za-z ]+) (\\d{2}\\.\\d{2}\\.\\d{4})_nb\\.pdf$")
    party <- info[2]
    person <- info[3]
    date <- as.Date(info[4], format = "%d.%m.%Y")
    year <- format(date, "%Y")
    
    cat("Extracted info - Party:", party, "Person:", person, "Date:", date, "Year:", year, "\n")
    
    if (!is.na(party) && !is.na(year)) {
      for (sentence in sentences) {
        data <- rbind(data, data.frame(party = party, year = year, text = sentence, stringsAsFactors = FALSE))
      }
    } else {
      cat("Warning: Missing data for file:", file_name, "\n")
    }
  }
  
  return(data)
}
