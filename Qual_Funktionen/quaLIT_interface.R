run_quaLIT <- function(aggregated_texts, categories, save_dir) {
  
  custom_theme <- create_theme(
    adminlte_color(
      light_blue = "#0f4a4e",
      blue = "#A9A9A9",
      navy = "#1E1E1E",
      red = "#FF3B30",
      green = "#34C759",
      yellow = "#FFCC00"
    ),
    adminlte_sidebar(
      dark_bg = "#2C2C2E",
      dark_hover_bg = "#38383A",
      dark_color = "#C0C0C0"
    ),
    adminlte_global(
      content_bg = "#38383A",
      box_bg = "#cce8d9",
      info_box_bg = "#7ccaca"
    )
  )
  
  enhanced_css <- tags$head(
    tags$style(HTML('
      :root {
        --accent-color: #C0C0C0;
        --accent-hover: #A9A9A9;
        --text-color: #d1e4e4;
        --bg-color: rgba(30, 30, 30, 0.95);
        --box-bg-color: rgba(45, 45, 45, 0.7);
        --box-border-color: rgba(255, 255, 255, 0.1);
        --comment-bg-color: rgba(60, 60, 60, 0.8);
        --category-box-color: rgba(50, 50, 50, 0.8);
        --success-color: #34C759;
        --warning-color: #FFCC00;
        --danger-color: #FF3B30;
      }

      /* Button text color */
      .btn-primary, .btn-success {
        color: white !important;
      }

      /* Statement text styling */
      .statement-output {
        font-size: 1.2em !important;
        font-weight: bold !important;
      }

      /* Progress text color */
      .progress-text {
        color: white !important;
      }

      /* Progress bar styling */
      .progress {
        background-color: var(--box-bg-color);
      }

      /* Copyright styling */
      .copyright-footer {
        text-align: center;
        padding: 10px;
        color: var(--text-color);
        font-size: 0.9em;
        margin-top: 20px;
        border-top: 1px solid var(--box-border-color);
      }
    '))
  )
  
  js_code <- tags$script(HTML('
    $(document).on("keydown", function (e) {
      if (e.key === "ArrowRight") {
        $("#nextpage").click();
      } else if (e.key === "ArrowLeft") {
        $("#previouspage").click();
      }
    });
  '))
  
  ui <- dashboardPage(
    dashboardHeader(title = "quaLIT"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Coding", tabName = "coding", icon = icon("pen")),
        menuItem("Kategorienbaum", tabName = "kategorienbaum", icon = icon("project-diagram")),
        menuItem("Reliability", tabName = "reliability", icon = icon("check-double")),
        menuItem("Analytics", tabName = "analytics", icon = icon("chart-line"),
                 menuSubItem("Code Co-occurrences", tabName = "cooccurrence"),
                 menuSubItem("Code Timeline", tabName = "timeline"),
                 menuSubItem("Code Distribution", tabName = "distribution"),
                 menuSubItem("Hierarchical View", tabName = "hierarchy")
        )
      )
    ),
    dashboardBody(
      use_theme(custom_theme),
      enhanced_css,
      js_code,
      tabItems(
        # Coding tab
        tabItem(
          tabName = "coding",
          class = "tab-pane",
          fluidRow(
            box(
              title = "Text Selection",
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                column(4,
                       selectInput("party_year", "Choose Party and Year:",
                                   choices = unique(paste(aggregated_texts$party, aggregated_texts$year, sep = " - "))
                       )
                ),
                column(4,
                       actionButton("load_text", "Load Selected Text",
                                    class = "btn btn-primary"
                       )
                ),
                column(4,
                       checkboxInput("context", "Show Context", value = FALSE)
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.load_text > 0",
            fluidRow(
              box(
                title = "Statement",
                solidHeader = TRUE,
                width = 12,
                htmlOutput("statement"),
                class = "statement-output"
              )
            ),
            fluidRow(
              lapply(seq_along(categories), function(i) {
                box(
                  title = names(categories)[i],
                  solidHeader = TRUE,
                  width = 4,
                  radioButtons(
                    paste0("code", i),
                    NULL,
                    choices = c("missing" = "", categories[[i]]),
                    selected = ""
                  )
                )
              })
            ),
            fluidRow(
              box(
                title = "Comment",
                solidHeader = TRUE,
                width = 12,
                textInput("comment", NULL, value = "")
              )
            ),
            fluidRow(
              column(width = 6, offset = 3,
                     div(style = "text-align: center; margin: 20px 0;",
                         actionButton("previouspage", "Previous",
                                      class = "btn btn-primary",
                                      icon = icon("backward")
                         ),
                         actionButton("nextpage", "Next",
                                      class = "btn btn-primary",
                                      icon = icon("forward")
                         ),
                         actionButton("connect_previous", "Connect with Previous",
                                      class = "btn btn-warning",
                                      icon = icon("link")
                         ),
                         actionButton("save_progress", "Save Progress",
                                      class = "btn btn-success",
                                      icon = icon("save")
                         ),
                         actionButton("save", "Save and Exit",
                                      class = "btn btn-primary",
                                      icon = icon("floppy-disk")
                         )
                     )
              )
            ),
            fluidRow(
              column(width = 12,
                     progressBar(
                       id = "progress",
                       value = 0,
                       total = 100,
                       size = "xs"
                     )
              )
            )
          ),
          fluidRow(
            box(
              title = "Text Canvas",
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              fluidRow(
                column(12,
                       div(style = "margin-bottom: 15px;",
                           actionButton("clear_canvas", "Clear Canvas", 
                                        class = "btn btn-warning",
                                        icon = icon("eraser")
                           ),
                           actionButton("save_canvas", "Save Notes", 
                                        class = "btn btn-success",
                                        icon = icon("save")
                           ),
                           downloadButton("download_canvas", "Download Notes",
                                          class = "btn btn-info"
                           )
                       )
                )
              ),
              fluidRow(
                column(12,
                       textAreaInput("canvas_notes",
                                     label = NULL,
                                     value = "",
                                     width = "100%",
                                     height = "200px",
                                     resize = "vertical"
                       )
                )
              )
            )
          )
        ),
        
        # Kategorienbaum tab
        tabItem(
          tabName = "kategorienbaum",
          class = "tab-pane",
          fluidRow(
            box(
              title = "Dataset Selection",
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                column(8,
                       selectInput("dataset_select", "Choose Dataset:",
                                   choices = list.files(file.path(save_dir, "Reden", "nb", "codings"), pattern = "\\.rds$")
                       )
                ),
                column(4,
                       div(style = "margin-top: 25px;",
                           actionButton("refresh_datasets", "Refresh Datasets", 
                                        icon = icon("sync"),
                                        class = "btn btn-primary"
                           ),
                           actionButton("save_graph", "Save Graph", 
                                        icon = icon("save"),
                                        class = "btn btn-success"
                           )
                       )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Kategorienbaum Visualization",
              width = 12,
              solidHeader = TRUE,
              visNetworkOutput("network_plot", height = "800px")
            )
          )
        ),
        
        # Reliability tab
        tabItem(
          tabName = "reliability",
          class = "tab-pane",
          fluidRow(
            box(
              title = "Reliability Analysis",
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                column(6,
                       selectInput("dataset_a", "Select First Dataset:",
                                   choices = NULL
                       )
                ),
                column(6,
                       selectInput("dataset_b", "Select Second Dataset:",
                                   choices = NULL
                       )
                )
              ),
              fluidRow(
                column(12,
                       actionButton("calculate_reliability", "Calculate Reliability",
                                    class = "btn btn-primary"
                       )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Results",
              width = 6,
              solidHeader = TRUE,
              verbatimTextOutput("reliability_results")
            ),
            box(
              title = "Frequency Analysis",
              width = 6,
              solidHeader = TRUE,
              tableOutput("frequency_table")
            )
          )
        ),
        
        # Co-occurrence tab
        tabItem(
          tabName = "cooccurrence",
          class = "tab-pane",
          fluidRow(
            box(
              title = "Dataset Selection",
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                column(8,
                       selectInput("analytics_dataset", "Choose Dataset:",
                                   choices = list.files(file.path(save_dir, "Reden", "nb", "codings"), pattern = "\\.rds$")
                       )
                ),
                column(4,
                       div(style = "margin-top: 25px;",
                           actionButton("refresh_analytics", "Refresh Datasets",
                                        icon = icon("sync"),
                                        class = "btn btn-primary"
                           )
                       )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Co-occurrence Heatmap",
              width = 12,
              solidHeader = TRUE,
              plotlyOutput("cooccurrence_plot"),
              div(style = "text-align: right; margin-top: 10px;",
                  actionButton("save_cooccurrence", "Save Heatmap",
                               icon = icon("save"),
                               class = "btn btn-success"
                  )
              )
            )
          )
        ),
        
        # Timeline tab
        tabItem(
          tabName = "timeline",
          class = "tab-pane",
          fluidRow(
            box(
              title = "Dataset Selection",
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                column(8,
                       selectInput("analytics_dataset_timeline", "Choose Dataset:",
                                   choices = list.files(file.path(save_dir, "Reden", "nb", "codings"), pattern = "\\.rds$")
                       )
                ),
                column(4,
                       div(style = "margin-top: 25px;",
                           actionButton("refresh_analytics_timeline", "Refresh Datasets",
                                        icon = icon("sync"),
                                        class = "btn btn-primary"
                           )
                       )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Code Timeline",
              width = 12,
              solidHeader = TRUE,
              plotlyOutput("timeline_plot"),
              div(style = "text-align: right; margin-top: 10px;",
                  actionButton("save_timeline", "Save Timeline",
                               icon = icon("save"),
                               class = "btn btn-success"
                  )
              )
            )
          )
        ),
        
        # Distribution tab
        tabItem(
          tabName = "distribution",
          class = "tab-pane",
          fluidRow(
            box(
              title = "Dataset Selection",
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                column(8,
                       selectInput("analytics_dataset_dist", "Choose Dataset:",
                                   choices = list.files(file.path(save_dir, "Reden", "nb", "codings"), pattern = "\\.rds$")
                       )
                ),
                column(4,
                       div(style = "margin-top: 25px;",
                           actionButton("refresh_analytics_dist", "Refresh Datasets",
                                        icon = icon("sync"),
                                        class = "btn btn-primary"
                           )
                       )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Code Distribution",
              width = 12,
              solidHeader = TRUE,
              plotlyOutput("distribution_plot"),
              div(style = "text-align: right; margin-top: 10px;",
                  actionButton("save_distribution", "Save Distribution",
                               icon = icon("save"),
                               class = "btn btn-success"
                  )
              )
            )
          )
        ),
        
        # Hierarchy tab
        tabItem(
          tabName = "hierarchy",
          class = "tab-pane",
          fluidRow(
            box(
              title = "Dataset Selection",
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                column(8,
                       selectInput("analytics_dataset_hier", "Choose Dataset:",
                                   choices = list.files(file.path(save_dir, "Reden", "nb", "codings"), pattern = "\\.rds$")
                       )
                ),
                column(4,
                       div(style = "margin-top: 25px;",
                           actionButton("refresh_analytics_hier", "Refresh Datasets",
                                        icon = icon("sync"),
                                        class = "btn btn-primary"
                           )
                       )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Hierarchical View",
              width = 12,
              solidHeader = TRUE,
              sunburstR::sunburstOutput("hierarchy_plot")
            )
          )
        )
      ),
      div(
        class = "copyright-footer",
        HTML(paste0(
          "(c) ", format(Sys.Date(), "%Y"), " quaLIT by Alexander Thiel - Monetization not permitted",
          "<br>",
          "Version 1.3 - beta"
        ))
      )
    )
  )

  
  generate_analytics <- function(data) {
    create_cooccurrence_matrix <- function(data) {
      hk_cols <- grep("^HK", names(data), value = TRUE)
      cooc_matrix <- matrix(0, nrow = length(hk_cols), ncol = length(hk_cols))
      rownames(cooc_matrix) <- colnames(cooc_matrix) <- hk_cols
      
      for(i in seq_along(hk_cols)) {
        for(j in seq_along(hk_cols)) {
          if(i != j) {
            cooc <- sum(!is.na(data[[hk_cols[i]]]) & !is.na(data[[hk_cols[j]]]) & 
                          data[[hk_cols[i]]] != "" & data[[hk_cols[j]]] != "")
            cooc_matrix[i,j] <- cooc
          }
        }
      }
      return(cooc_matrix)
    }
    
    create_timeline_data <- function(data) {
      hk_cols <- grep("^HK", names(data), value = TRUE)
      timeline_data <- data.frame(
        index = 1:nrow(data),
        stringsAsFactors = FALSE
      )
      
      for(col in hk_cols) {
        timeline_data[[col]] <- ifelse(data[[col]] != "" & !is.na(data[[col]]), 1, 0)
      }
      
      return(timeline_data)
    }
    
    create_distribution_data <- function(data) {
      hk_cols <- grep("^HK", names(data), value = TRUE)
      dist_data <- list()
      
      for(col in hk_cols) {
        valid_values <- data[[col]][data[[col]] != "" & !is.na(data[[col]])]
        
        if(length(valid_values) > 0) {
          values <- table(valid_values)
          dist_data[[col]] <- data.frame(
            category = names(values),
            count = as.numeric(values),
            stringsAsFactors = FALSE
          )
        } else {
          dist_data[[col]] <- data.frame(
            category = "No codes",
            count = 0,
            stringsAsFactors = FALSE
          )
        }
      }
      
      return(dist_data)
    }
    
    create_hierarchy_data <- function(data) {
      hk_cols <- grep("^HK", names(data), value = TRUE)
      
      # Schwellenwerte für die Ringzuordnung
      frequency_thresholds <- list(
        inner = 0.75,    # über 20% -> Ring 1
        middle = 0.3,   # 10-20% -> Ring 2
        outer = 0.05,    # 5-10% -> Ring 3
        furthest = 0     # unter 5% -> Ring 4
      )
      
      hier_data <- list(
        name = "root",
        children = lapply(hk_cols, function(col) {
          # Werte und ihre Häufigkeiten
          values <- table(data[[col]][data[[col]] != "" & !is.na(data[[col]])])
          max_freq <- max(values)
          
          # Sortiere Werte nach Häufigkeit und berechne relative Häufigkeiten
          sorted_values <- sort(values, decreasing = TRUE)
          relative_freq <- sorted_values / max_freq
          
          # Gruppiere Werte nach Häufigkeit
          ring1_values <- names(relative_freq[relative_freq >= frequency_thresholds$inner])
          ring2_values <- names(relative_freq[relative_freq >= frequency_thresholds$middle & 
                                                relative_freq < frequency_thresholds$inner])
          ring3_values <- names(relative_freq[relative_freq >= frequency_thresholds$outer & 
                                                relative_freq < frequency_thresholds$middle])
          ring4_values <- names(relative_freq[relative_freq < frequency_thresholds$outer])
          
          # Erstelle hierarchische Struktur
          list(
            name = col,
            children = c(
              # Ring 1: Häufigste Werte
              lapply(ring1_values, function(val) {
                list(
                  name = val,
                  children = NULL,  # Keine weiteren Kinder
                  size = as.numeric(values[val]),
                  percentage = round(100 * values[val] / sum(values), 2),
                  rank = which(names(sorted_values) == val)
                )
              }),
              # Ring 2: Mittelhäufige Werte als Gruppe 1
              if(length(ring2_values) > 0) {
                list(list(
                  name = "Mittelhäufig",
                  children = lapply(ring2_values, function(val) {
                    list(
                      name = val,
                      size = as.numeric(values[val]),
                      percentage = round(100 * values[val] / sum(values), 2),
                      rank = which(names(sorted_values) == val)
                    )
                  })
                ))
              },
              # Ring 3: Seltenere Werte als Gruppe 2
              if(length(ring3_values) > 0) {
                list(list(
                  name = "Seltener",
                  children = lapply(ring3_values, function(val) {
                    list(
                      name = val,
                      children = list(
                        list(
                          name = paste(val, "(", values[val], ")", sep=""),
                          size = as.numeric(values[val]),
                          percentage = round(100 * values[val] / sum(values), 2),
                          rank = which(names(sorted_values) == val)
                        )
                      )
                    )
                  })
                ))
              },
              # Ring 4: Seltenste Werte als Gruppe 3
              if(length(ring4_values) > 0) {
                list(list(
                  name = "Sehr selten",
                  children = lapply(ring4_values, function(val) {
                    list(
                      name = val,
                      children = list(
                        list(
                          name = paste(val, "(", values[val], ")", sep=""),
                          size = as.numeric(values[val]),
                          percentage = round(100 * values[val] / sum(values), 2),
                          rank = which(names(sorted_values) == val)
                        )
                      )
                    )
                  })
                ))
              }
            )
          )
        })
      )
      return(hier_data)
    }
    
    return(list(
      cooccurrence = create_cooccurrence_matrix(data),
      timeline = create_timeline_data(data),
      distribution = create_distribution_data(data),
      hierarchy = create_hierarchy_data(data)
    ))
  }
  
  server <- function(input, output, session) {
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    showModal(modalDialog(
      title = "Enter Coder's Name",
      textInput("coder_name_input", "Name:", value = ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_name", "Submit")
      )
    ))
    
    values <- reactiveValues(
      counter = 1,
      selected_text = NULL,
      coding_data = NULL,
      coder = NULL,
      merged_statements = list()
    )
    
    observeEvent(input$submit_name, {
      values$coder <- input$coder_name_input
      removeModal()
    })
    
    standardize_column_names <- function(data) {
      standard_cols <- c("texts")
      hk_cols <- grep("HK", names(data), value = TRUE)
      new_names <- paste0("HK", seq_along(hk_cols))
      name_mapping <- setNames(new_names, hk_cols)
      name_mapping <- c(texts = "texts", name_mapping, comments = "comments")
      data_standard <- data
      names(data_standard) <- name_mapping[names(data)]
      return(data_standard)
    }
    
    save_current_data <- function() {
      req(values$selected_text, values$counter, values$coding_data)
      if(!is.null(values$coding_data)) {
        for (i in seq_along(categories)) {
          current_code <- input[[paste0("code", i)]]
          if(!is.null(current_code)) {
            values$coding_data[values$counter, i + 1] <- current_code
          }
        }
        values$coding_data$comments[values$counter] <- input$comment %||% ""
        
        values$coding_data$texts <- values$selected_text
        
        if(!is.null(input$party_year)) {
          selected <- strsplit(input$party_year, " - ")[[1]]
          filename <- paste0(
            values$coder %||% "unnamed_coder",
            "_",
            selected[1],
            "_",
            selected[2],
            "_data.rds"
          )
          save_path <- file.path(save_dir, "Reden", "nb", "codings")
          dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
          
          standardized_data <- standardize_column_names(values$coding_data)
          
          tryCatch({
            saveRDS(
              standardized_data,
              file.path(save_path, filename)
            )
            showNotification("Data saved successfully", type = "message")
          }, error = function(e) {
            showNotification(paste("Error saving data:", e$message), type = "error")
          })
        }
      }
    }
    
    observeEvent(input$save_progress, {
      save_current_data()
    })
    
    observeEvent(input$save, {
      save_current_data()
      stopApp(invisible(values$coding_data))
    })
    
    observeEvent(input$load_text, {
      req(input$party_year)
      selected <- strsplit(input$party_year, " - ")[[1]]
      party <- selected[1]
      year <- selected[2]
      text <- aggregated_texts$text[aggregated_texts$party == party & aggregated_texts$year == year]
      
      if(length(text) > 0) {
        values$selected_text <- text
        values$counter <- 1
        values$coding_data <- data.frame(
          texts = text,
          matrix(NA, nrow = length(text), ncol = length(categories),
                 dimnames = list(NULL, names(categories))),
          comments = rep("", length(text)),
          stringsAsFactors = FALSE
        )
        updateProgressBar(session, "progress", value = 1, total = length(text))
        
        for (i in seq_along(categories)) {
          updateRadioButtons(session, paste0("code", i), selected = "")
        }
        updateTextInput(session, "comment", value = "")
      }
    })
    
    output$statement <- renderText({
      req(values$selected_text, values$counter)
      
      if (!is.null(input$context) && input$context) {
        before_text <- ifelse(values$counter > 1, paste0('<font color="#C0C0C0">', values$selected_text[values$counter - 1], '</font> '), "")
        after_text <- ifelse(values$counter < length(values$selected_text), paste0(' <font color="#C0C0C0">', values$selected_text[values$counter + 1], '</font>'), "")
        current_text <- paste0(before_text, '<b>', values$selected_text[values$counter], '</b>', after_text)
      } else {
        current_text <- values$selected_text[values$counter]
      }
      
      paste0(
        '<div class="statement-output">',
        current_text,
        '<br><br><span class="progress-text">',
        values$counter, ' / ', length(values$selected_text),
        '</span></div>'
      )
    })
    
    observeEvent(input$connect_previous, {
      req(values$selected_text, values$counter)
      if (values$counter > 1) {
        if(!is.null(values$coding_data)) {
          for (i in seq_along(categories)) {
            current_code <- input[[paste0("code", i)]]
            if(!is.null(current_code)) {
              values$coding_data[values$counter, i + 1] <- current_code
            }
          }
          values$coding_data$comments[values$counter] <- input$comment %||% ""
        }
        
        values$selected_text[values$counter - 1] <- paste(values$selected_text[values$counter - 1], values$selected_text[values$counter])
        values$coding_data$texts[values$counter - 1] <- paste(values$coding_data$texts[values$counter - 1], values$coding_data$texts[values$counter])
        
        values$selected_text <- values$selected_text[-values$counter]
        values$coding_data <- values$coding_data[-values$counter, , drop = FALSE]
        
        values$counter <- values$counter - 1
        updateProgressBar(session, "progress", value = values$counter, total = length(values$selected_text))
        
        for (i in seq_along(categories)) {
          updateRadioButtons(session, paste0("code", i), 
                             selected = ifelse(is.na(values$coding_data[values$counter, i + 1]), 
                                               "", 
                                               values$coding_data[values$counter, i + 1]))
        }
        updateTextInput(session, "comment", value = values$coding_data$comments[values$counter])
        
        save_current_data()
      }
    })
    
    observeEvent(input$previouspage, {
      req(values$selected_text, values$counter)
      if (values$counter > 1) {
        if(!is.null(values$coding_data)) {
          for (i in seq_along(categories)) {
            current_code <- input[[paste0("code", i)]]
            if(!is.null(current_code)) {
              values$coding_data[values$counter, i + 1] <- current_code
            }
          }
          values$coding_data$comments[values$counter] <- input$comment %||% ""
        }
        values$counter <- values$counter - 1
        updateProgressBar(session, "progress",
                          value = values$counter,
                          total = length(values$selected_text))
        for (i in seq_along(categories)) {
          updateRadioButtons(session, paste0("code", i), selected = "")
        }
        
        updateTextInput(session, "comment",
                        value = values$coding_data$comments[values$counter])
      }
    })
    
    observeEvent(input$nextpage, {
      req(values$selected_text, values$counter)
      if (!is.null(values$selected_text) && values$counter <= length(values$selected_text)) {
        if(!is.null(values$coding_data)) {
          for (i in seq_along(categories)) {
            current_code <- input[[paste0("code", i)]]
            if(!is.null(current_code)) {
              values$coding_data[values$counter, i + 1] <- current_code
            }
          }
          values$coding_data$comments[values$counter] <- input$comment %||% ""
        }
        if (values$counter < length(values$selected_text)) {
          values$counter <- values$counter + 1
          updateProgressBar(session, "progress",
                            value = values$counter,
                            total = length(values$selected_text))
          for (i in seq_along(categories)) {
            updateRadioButtons(session, paste0("code", i), selected = "")
          }
          
          updateTextInput(session, "comment",
                          value = values$coding_data$comments[values$counter])
        }
      }
    })
    
    nodes <- reactive({
      req(input$dataset_select)
      data <- readRDS(file.path(save_dir, "Reden", "nb", "codings", input$dataset_select))
      
      clean_value <- function(x) {
        return(!is.na(x) & x != "" & x != "_Not applicable_")
      }
      
      hk_cols <- grep("^HK", names(data), value = TRUE)
      
      hk_colors <- setNames(rainbow(length(hk_cols)), hk_cols)
      
      nodes_hk <- data.frame(
        id = hk_cols,
        label = hk_cols,
        group = hk_cols,
        value = sapply(data[hk_cols], function(x) length(unique(x[clean_value(x)]))),
        color = hk_colors[hk_cols],
        shape = "square",
        stringsAsFactors = FALSE
      )
      
      nodes_values_list <- list()
      
      for(col in hk_cols) {
        values <- unique(data[[col]][clean_value(data[[col]])])
        if(length(values) > 0) {
          nodes_values_list[[col]] <- data.frame(
            id = values,
            label = values,
            group = col,
            value = sapply(values, function(v) sum(data[[col]] == v, na.rm = TRUE)),
            color = hk_colors[col],
            shape = "dot",
            stringsAsFactors = FALSE
          )
        }
      }
      
      nodes_values <- do.call(rbind, nodes_values_list)
      rbind(nodes_hk, nodes_values)
    })
    
    edges <- reactive({
      req(input$dataset_select)
      data <- readRDS(file.path(save_dir, "Reden", "nb", "codings", input$dataset_select))
      
      clean_value <- function(x) {
        return(!is.na(x) & x != "" & x != "_Not applicable_")
      }
      
      hk_cols <- grep("^HK", names(data), value = TRUE)
      hk_colors <- setNames(rainbow(length(hk_cols)), hk_cols)
      
      edges_standard_list <- list()
      for(col in hk_cols) {
        values <- unique(data[[col]][clean_value(data[[col]])])
        if(length(values) > 0) {
          edges_standard_list[[col]] <- data.frame(
            from = col,
            to = values,
            width = 1,
            color = hk_colors[col],
            dashes = FALSE,
            arrows = "to",
            stringsAsFactors = FALSE
          )
        }
      }
      
      edges_interactions <- NULL
      if("comments" %in% names(data)) {
        valid_comments <- data$comments[clean_value(data$comments)]
        
        if(length(valid_comments) > 0) {
          interactions_list <- lapply(valid_comments, function(comment) {
            parts <- unlist(strsplit(comment, "x"))
            if(length(parts) >= 2) {
              edges <- data.frame()
              for(i in 1:(length(parts)-1)) {
                from <- parts[i]
                to <- parts[i+1]
                
                is_from_hk <- grepl("^HK", from)
                is_to_hk <- grepl("^HK", to)
                
                edges <- rbind(edges, data.frame(
                  from = from,
                  to = to,
                  width = 2,
                  color = "#666666",
                  dashes = (is_from_hk && is_to_hk),
                  arrows = "to",
                  stringsAsFactors = FALSE
                ))
              }
              return(edges)
            }
            return(NULL)
          })
          
          edges_interactions <- do.call(rbind, interactions_list)
          
          if(!is.null(edges_interactions)) {
            edges_interactions <- edges_interactions %>%
              dplyr::group_by(from, to) %>%
              dplyr::summarise(
                width = dplyr::n() * 2,
                color = dplyr::first(color),
                dashes = dplyr::first(dashes),
                arrows = dplyr::first(arrows),
                .groups = 'drop'
              )
          }
        }
      }
      
      edges_standard <- do.call(rbind, edges_standard_list)
      if(is.null(edges_interactions)) {
        return(edges_standard)
      } else {
        return(dplyr::bind_rows(edges_standard, edges_interactions))
      }
    })
    
    output$network_plot <- renderVisNetwork({
      visNetwork(nodes(), edges()) %>%
        visOptions(
          highlightNearest = list(
            enabled = TRUE,
            degree = 2,
            hover = TRUE
          ),
          nodesIdSelection = TRUE
        ) %>%
        visPhysics(
          solver = "forceAtlas2Based",
          forceAtlas2Based = list(
            gravitationalConstant = -50,
            centralGravity = 0.01,
            springLength = 100,
            springConstant = 0.08
          ),
          stabilization = list(
            enabled = TRUE,
            iterations = 1000
          )
        ) %>%
        visLayout(randomSeed = 123) %>%
        visInteraction(
          navigationButtons = TRUE,
          keyboard = TRUE
        ) %>%
        visExport(
          type = "png",
          name = "network-export",
          float = "left",
          background = "white"
        )
    })
    
    observeEvent(input$refresh_datasets, {
      updated_files <- list.files(file.path(save_dir, "Reden", "nb", "codings"), pattern = "\\.rds$")
      updateSelectInput(session, "dataset_select", choices = updated_files)
      updateSelectInput(session, "dataset_a", choices = updated_files)
      updateSelectInput(session, "dataset_b", choices = updated_files)
      showNotification("Dataset list refreshed", type = "message")
    })
    
    observeEvent(input$save_graph, {
      req(input$dataset_select)
      save_path <- file.path(save_dir, "Reden", "nb", "graphs")
      dir.create(save_path, showWarnings = FALSE)
      
      filename <- paste0(
        tools::file_path_sans_ext(input$dataset_select),
        "_graph_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".png"
      )
      
      network <- visNetwork(nodes(), edges()) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE),
          nodesIdSelection = TRUE
        ) %>%
        visPhysics(
          solver = "forceAtlas2Based",
          forceAtlas2Based = list(
            gravitationalConstant = -50,
            centralGravity = 0.01,
            springLength = 100,
            springConstant = 0.08
          ),
          stabilization = list(enabled = TRUE, iterations = 1000)
        ) %>%
        visLayout(randomSeed = 123) %>%
        visInteraction(navigationButtons = TRUE, keyboard = TRUE)
      
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(network, temp_html)
      webshot::webshot(
        temp_html,
        file = file.path(save_path, filename),
        delay = 2,
        vwidth = 1000,
        vheight = 800
      )
      unlink(temp_html)
      
      showNotification(paste("Graph saved as:", filename), type = "message")
    })
    
    observe({
      codings_path <- file.path(save_dir, "Reden", "nb", "codings")
      files <- list.files(codings_path, pattern = "\\.rds$")
      updateSelectInput(session, "dataset_a", choices = files)
      updateSelectInput(session, "dataset_b", choices = files)
    })
    
    observeEvent(input$calculate_reliability, {
      req(input$dataset_a, input$dataset_b)
      codings_path <- file.path(save_dir, "Reden", "nb", "codings")
      dataset_a <- readRDS(file.path(codings_path, input$dataset_a))
      dataset_b <- readRDS(file.path(codings_path, input$dataset_b))
      
      hk_cols_a <- grep("^HK", names(dataset_a), value = TRUE)
      hk_cols_b <- grep("^HK", names(dataset_b), value = TRUE)
      
      if (!identical(hk_cols_a, hk_cols_b)) {
        output$reliability_results <- renderPrint({
          cat("Error: Datasets have different structures or coding categories.")
        })
        return()
      }
      
      if (nrow(dataset_a) != nrow(dataset_b)) {
        output$reliability_results <- renderPrint({
          cat("Error: Datasets have different numbers of coded items.")
        })
        return()
      }
      
      output$cooccurrence_plot <- renderPlotly({
        req(input$analytics_dataset)
        data <- readRDS(file.path(save_dir, "Reden", "nb", "codings", input$analytics_dataset))
        analytics <- generate_analytics(data)
        
        plot_ly(
          x = colnames(analytics$cooccurrence),
          y = rownames(analytics$cooccurrence),
          z = analytics$cooccurrence,
          type = "heatmap",
          colors = green_palette(100)
        ) %>%
          layout(
            title = "Code Co-occurrence Matrix",
            xaxis = list(title = ""),
            yaxis = list(title = "")
          )
      })
      
      output$timeline_plot <- renderPlotly({
        req(input$analytics_dataset_timeline)
        data <- readRDS(file.path(save_dir, "Reden", "nb", "codings", input$analytics_dataset_timeline))
        analytics <- generate_analytics(data)
        
        timeline_long <- reshape2::melt(analytics$timeline, id.vars = "index")
        
        plot_ly(data = timeline_long, x = ~index, y = ~value, color = ~variable,
                type = "scatter", mode = "lines+markers",
                colors = green_palette(length(unique(timeline_long$variable)))) %>%
          layout(
            title = "Code Timeline",
            xaxis = list(title = "Text Sequence"),
            yaxis = list(title = "Code Present")
          )
      })
      
      output$distribution_plot <- renderPlotly({
        req(input$analytics_dataset_dist)
        data <- readRDS(file.path(save_dir, "Reden", "nb", "codings", input$analytics_dataset_dist))
        analytics <- generate_analytics(data)
        dist_data <- do.call(rbind, lapply(names(analytics$distribution), function(name) {
          if(nrow(analytics$distribution[[name]]) > 0) {
            cbind(HK = name, analytics$distribution[[name]])
          }
        }))
        if(nrow(dist_data) > 0) {
          plot_ly(data = dist_data, x = ~HK, y = ~count, color = ~category,
                  type = "bar",
                  colors = green_palette(length(unique(dist_data$category)))) %>%
            layout(
              title = "Code Distribution",
              barmode = "stack",
              xaxis = list(title = "Categories"),
              yaxis = list(title = "Count")
            )
        } else {
          plot_ly() %>%
            layout(
              title = "No coding data available",
              xaxis = list(title = "Categories"),
              yaxis = list(title = "Count")
            )
        }
      })
      
      
      output$reliability_results <- renderPrint({
        capture.output({
          calculate_reliability(dataset_a, dataset_b)
        }) -> results
        cat("Reliability Analysis Results:\n")
        cat("============================\n\n")
        cat(paste(results, collapse = "\n"))
      })
      
      output$frequency_table <- renderTable({
        frequency_df <- data.frame()
        
        for(col in hk_cols_a) {
          values_a <- dataset_a[[col]][dataset_a[[col]] != "" & !is.na(dataset_a[[col]])]
          values_b <- dataset_b[[col]][dataset_b[[col]] != "" & !is.na(dataset_b[[col]])]
          
          all_categories <- unique(c(values_a, values_b))
          all_categories <- all_categories[all_categories != "_Not applicable_"]
          
          if(length(all_categories) > 0) {
            for(cat in all_categories) {
              count_a <- sum(values_a == cat, na.rm = TRUE)
              count_b <- sum(values_b == cat, na.rm = TRUE)
              
              frequency_df <- rbind(frequency_df, data.frame(
                categories = col,
                expression = cat,
                `Count Coder 1` = count_a,
                `Count Coder 2` = count_b
              ))
            }
            
            frequency_df <- rbind(frequency_df, data.frame(
              categories = col,
              expression = "Total count",
              `Count Coder 1` = length(values_a),
              `Count Coder 2` = length(values_b)
            ))
            
            counts_a <- table(values_a)
            counts_b <- table(values_b)
            
            diff_a <- if(length(counts_a) > 0) max(counts_a) - min(counts_a) else 0
            diff_b <- if(length(counts_b) > 0) max(counts_b) - min(counts_b) else 0
            
            frequency_df <- rbind(frequency_df, data.frame(
              categories = col,
              expression = "Difference lowest to highest",
              `Count Coder 1` = diff_a,
              `Count Coder 2` = diff_b
            ))
            
            frequency_df <- rbind(frequency_df, data.frame(
              categories = "",
              expression = "",
              `Count Coder 1` = NA,
              `Count Coder 2` = NA
            ))
          }
        }
        frequency_df
      }, na = "")
    })
    
    observeEvent(input$refresh_analytics, {
      updated_files <- list.files(file.path(save_dir, "Reden", "nb", "codings"), pattern = "\\.rds$")
      updateSelectInput(session, "analytics_dataset", choices = updated_files)
      showNotification("Analytics dataset list refreshed", type = "message")
    })
    
    green_palette <- colorRampPalette(c("#f0fff0", "#90EE90", "#006400"))
    
    output$cooccurrence_plot <- renderPlotly({
      req(input$analytics_dataset)
      data <- readRDS(file.path(save_dir, "Reden", "nb", "codings", input$analytics_dataset))
      analytics <- generate_analytics(data)
      
      plot_ly(
        x = colnames(analytics$cooccurrence),
        y = rownames(analytics$cooccurrence),
        z = analytics$cooccurrence,
        type = "heatmap",
        colors = green_palette(100)
      ) %>%
        layout(
          title = "Code Co-occurrence Matrix",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
    
    output$timeline_plot <- renderPlotly({
      req(input$analytics_dataset_timeline)
      data <- readRDS(file.path(save_dir, "Reden", "nb", "codings", input$analytics_dataset_timeline))
      analytics <- generate_analytics(data)
      
      timeline_long <- melt(analytics$timeline, id.vars = "index")
      
      plot_ly(data = timeline_long, x = ~index, y = ~value, color = ~variable,
              type = "scatter", mode = "lines+markers",
              colors = green_palette(length(unique(timeline_long$variable)))) %>%
        layout(
          title = "Code Timeline",
          xaxis = list(title = "Text Sequence"),
          yaxis = list(title = "Code Present")
        )
    })
    
    output$distribution_plot <- renderPlotly({
      req(input$analytics_dataset_dist)
      data <- readRDS(file.path(save_dir, "Reden", "nb", "codings", input$analytics_dataset_dist))
      analytics <- generate_analytics(data)
      
      dist_data <- do.call(rbind, lapply(names(analytics$distribution), function(name) {
        cbind(HK = name, analytics$distribution[[name]])
      }))
      
      plot_ly(data = dist_data, x = ~HK, y = ~count, color = ~category,
              type = "bar",
              colors = green_palette(length(unique(dist_data$category)))) %>%
        layout(
          title = "Code Distribution",
          barmode = "stack",
          xaxis = list(title = "Categories"),
          yaxis = list(title = "Count")
        )
    })
    
    cached_data <- memoise::memoise(function(file_path) {
      readRDS(file_path)
    })
    
    output$hierarchy_plot <- sunburstR::renderSunburst({
      req(input$analytics_dataset_hier)
      
      file_path <- file.path(save_dir, "Reden", "nb", "codings", input$analytics_dataset_hier)
      data <- cached_data(file_path)
      analytics <- generate_analytics(data)
      
      sunburstR::sunburst(
        analytics$hierarchy,
        colors = green_palette(100),
        count = TRUE,
        percent = TRUE,
        breadcrumb = TRUE
      )
    })
    
    
    observeEvent(input$save_cooccurrence, {
      req(input$analytics_dataset)
      save_path <- file.path(save_dir, "Reden", "nb", "analytics")
      dir.create(save_path, showWarnings = FALSE)
      
      filename <- paste0(
        tools::file_path_sans_ext(input$analytics_dataset),
        "_cooccurrence_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".png"
      )
      
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(plotly::last_plot(), temp_html)
      webshot::webshot(
        temp_html,
        file = file.path(save_path, filename),
        delay = 2,
        vwidth = 1000,
        vheight = 800
      )
      
      unlink(temp_html)
      
      showNotification(paste("Cooccurrence plot saved as:", filename), type = "message")
    })
    
    observeEvent(input$save_timeline, {
      req(input$analytics_dataset_timeline)
      save_path <- file.path(save_dir, "Reden", "nb", "analytics")
      dir.create(save_path, showWarnings = FALSE)
      
      filename <- paste0(
        tools::file_path_sans_ext(input$analytics_dataset_timeline),
        "_timeline_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".png"
      )
      
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(plotly::last_plot(), temp_html)
      webshot::webshot(
        temp_html,
        file = file.path(save_path, filename),
        delay = 2,
        vwidth = 1000,
        vheight = 800
      )
      
      unlink(temp_html)
      
      showNotification(paste("Timeline plot saved as:", filename), type = "message")
    })
    
    observeEvent(input$save_distribution, {
      req(input$analytics_dataset_dist)
      save_path <- file.path(save_dir, "Reden", "nb", "analytics")
      dir.create(save_path, showWarnings = FALSE)
      
      filename <- paste0(
        tools::file_path_sans_ext(input$analytics_dataset_dist),
        "_distribution_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".png"
      )
      
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(plotly::last_plot(), temp_html)
      webshot::webshot(
        temp_html,
        file = file.path(save_path, filename),
        delay = 2,
        vwidth = 1000,
        vheight = 800
      )
      
      unlink(temp_html)
      
      showNotification(paste("Distribution plot saved as:", filename), type = "message")
    })

    values$canvas_notes <- NULL
    observeEvent(input$clear_canvas, {
      showModal(modalDialog(
        title = "Clear Canvas",
        "Are you sure you want to clear all notes?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_clear", "Clear", class = "btn btn-warning")
        )
      ))
    })
    
    observeEvent(input$confirm_clear, {
      updateTextAreaInput(session, "canvas_notes", value = "")
      values$canvas_notes <- NULL
      removeModal()
      showNotification("Canvas cleared", type = "warning")
    })
    
    observeEvent(input$save_canvas, {
      req(input$canvas_notes, input$party_year)
      
      selected <- strsplit(input$party_year, " - ")[[1]]
      filename <- paste0(
        values$coder %||% "unnamed_coder",
        "_",
        selected[1],
        "_",
        selected[2],
        "_canvas_notes.txt"
      )
      
      save_path <- file.path(save_dir, "Reden", "nb", "canvas_notes")
      dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

      notes_text <- paste0(
        "Notes created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
        "Party-Year: ", input$party_year, "\n",
        "Coder: ", values$coder, "\n",
        "----------------------------------------\n\n",
        input$canvas_notes
      )
      
      writeLines(notes_text, file.path(save_path, filename))
      values$canvas_notes <- input$canvas_notes
      
      showNotification("Canvas notes saved successfully", type = "message")
    })

    output$download_canvas <- downloadHandler(
      filename = function() {
        req(input$party_year)
        selected <- strsplit(input$party_year, " - ")[[1]]
        paste0(
          values$coder %||% "unnamed_coder",
          "_",
          selected[1],
          "_",
          selected[2],
          "_canvas_notes_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".txt"
        )
      },
      content = function(file) {
        notes_text <- paste0(
          "Notes exported: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "Party-Year: ", input$party_year, "\n",
          "Coder: ", values$coder, "\n",
          "----------------------------------------\n\n",
          input$canvas_notes
        )
        writeLines(notes_text, file)
      }
    )
    
    observeEvent(input$load_text, {
      req(input$party_year)
      selected <- strsplit(input$party_year, " - ")[[1]]
      filename <- paste0(
        values$coder %||% "unnamed_coder",
        "_",
        selected[1],
        "_",
        selected[2],
        "_canvas_notes.txt"
      )
      
      notes_path <- file.path(save_dir, "Reden", "nb", "canvas_notes", filename)
      
      if (file.exists(notes_path)) {
        notes_content <- readLines(notes_path, warn = FALSE)
        notes_content <- notes_content[-(1:4)]
        updateTextAreaInput(session, "canvas_notes", 
                            value = paste(notes_content, collapse = "\n")
        )
        values$canvas_notes <- paste(notes_content, collapse = "\n")
      } else {
        updateTextAreaInput(session, "canvas_notes", value = "")
        values$canvas_notes <- NULL
      }
    })
  }
  
  shinyApp(ui, server)
}