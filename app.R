library(shiny)

normalize_cell <- function(value) {
  tolower(trimws(ifelse(is.na(value), "", value)))
}

is_empty_cell <- function(value) {
  normalize_cell(value) == ""
}

read_csv_grid <- function(path, separator) {
  data <- read.table(
    path,
    header = FALSE,
    sep = separator,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    fill = TRUE,
    blank.lines.skip = FALSE,
    comment.char = "",
    na.strings = character()
  )

  as.matrix(data)
}

find_cell <- function(grid, target) {
  matches <- which(normalize_cell(grid) == normalize_cell(target), arr.ind = TRUE)

  if (nrow(matches) == 0) {
    return(NULL)
  }

  list(row = matches[1, "row"], col = matches[1, "col"])
}

last_data_row <- function(grid, start_row, left_col, right_col) {
  selected_rows <- grid[start_row:nrow(grid), left_col:right_col, drop = FALSE]
  blank_rows <- apply(selected_rows, 1, function(row) all(is_empty_cell(row)))

  if (any(blank_rows)) {
    return(start_row + which(blank_rows)[1] - 2)
  }

  nrow(grid)
}

extract_target_data <- function(grid) {
  date_of_birth <- find_cell(grid, "Date of Birth")
  tags <- find_cell(grid, "Tags")

  if (is.null(date_of_birth)) {
    stop("Could not find a cell containing 'Date of Birth'.", call. = FALSE)
  }

  if (is.null(tags)) {
    stop("Could not find a cell containing 'Tags'.", call. = FALSE)
  }

  top_row <- date_of_birth$row
  left_col <- date_of_birth$col + 1

  if (left_col > ncol(grid)) {
    stop("'Date of Birth' was found in the last column, so there is no cell to its right.", call. = FALSE)
  }

  scan_cols <- if (tags$col < ncol(grid)) {
    seq.int(tags$col + 1, ncol(grid))
  } else {
    integer(0)
  }
  non_empty_cols <- scan_cols[!is_empty_cell(grid[tags$row, scan_cols])]

  if (length(non_empty_cols) == 0) {
    right_col <- ncol(grid)
  } else {
    right_col <- non_empty_cols[1] - 1
  }

  top_right_row <- tags$row + 1

  if (top_right_row != top_row) {
    warning(
      "The cell to the right of 'Date of Birth' and the calculated top-right cell are on different rows.",
      call. = FALSE
    )
  }

  if (right_col < left_col) {
    stop("The calculated right edge is left of the calculated left edge.", call. = FALSE)
  }

  bottom_row <- last_data_row(grid, top_row, left_col, right_col)

  if (bottom_row < top_row) {
    stop("No non-empty data was found in the calculated extraction area.", call. = FALSE)
  }

  extracted <- grid[top_row:bottom_row, left_col:right_col, drop = FALSE]
  extracted[is.na(extracted)] <- ""

  as.data.frame(
    extracted,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    optional = TRUE
  )
}

build_frequency_tables <- function(extracted, output_id_prefix) {
  if (nrow(extracted) == 0) {
    stop("The extracted area does not contain a header row.", call. = FALSE)
  }

  column_titles <- trimws(as.character(extracted[1, , drop = TRUE]))
  blank_titles <- column_titles == "" | is.na(column_titles)
  column_titles[blank_titles] <- paste("Column", which(blank_titles))

  data_rows <- extracted[-1, , drop = FALSE]

  lapply(seq_along(column_titles), function(column_index) {
    values <- if (nrow(data_rows) == 0) {
      character()
    } else {
      as.character(data_rows[[column_index]])
    }

    values[is.na(values) | trimws(values) == ""] <- "(blank)"
    counts <- sort(table(values), decreasing = TRUE)

    frequency_data <- if (length(counts) == 0) {
      data.frame(Value = "(no data)", Frequency = 0, check.names = FALSE)
    } else {
      data.frame(
        Value = names(counts),
        Frequency = as.integer(counts),
        check.names = FALSE
      )
    }

    list(
      id = paste0(output_id_prefix, "_frequency_", column_index),
      title = column_titles[column_index],
      data = frequency_data
    )
  })
}

frequency_color <- function(value) {
  if (value <= 10) {
    return("#f8d7da")
  }

  if (value < 20) {
    return("#fff3cd")
  }

  "#d4edda"
}

format_frequency <- function(value) {
  if (isTRUE(all.equal(value, round(value)))) {
    return(as.character(round(value)))
  }

  format(round(value, 2), nsmall = 2, trim = TRUE)
}

data_frame_table <- function(data) {
  tags$table(
    class = "table table-striped table-bordered table-hover",
    tags$thead(
      tags$tr(lapply(names(data), tags$th))
    ),
    tags$tbody(
      lapply(seq_len(nrow(data)), function(row_index) {
        tags$tr(
          lapply(seq_along(data), function(column_index) {
            tags$td(as.character(data[[column_index]][row_index]))
          })
        )
      })
    )
  )
}

frequency_table_html <- function(data, percentage) {
  if (!"Frequency" %in% names(data)) {
    return(data_frame_table(data))
  }

  adjusted <- data$Frequency * percentage / 100
  display_data <- data
  display_data$Frequency <- vapply(adjusted, format_frequency, character(1))

  tags$table(
    class = "table table-striped table-bordered table-hover",
    tags$thead(
      tags$tr(lapply(names(display_data), tags$th))
    ),
    tags$tbody(
      lapply(seq_len(nrow(display_data)), function(row_index) {
        tags$tr(
          lapply(seq_along(display_data), function(column_index) {
            if (names(display_data)[column_index] == "Frequency") {
              tags$td(
                style = paste0("background-color: ", frequency_color(adjusted[row_index]), ";"),
                display_data[[column_index]][row_index]
              )
            } else {
              tags$td(as.character(display_data[[column_index]][row_index]))
            }
          })
        )
      })
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .frequency-grid {
        display: grid;
        gap: 16px;
        grid-template-columns: repeat(3, minmax(0, 1fr));
      }

      .frequency-table {
        min-width: 0;
      }

      .frequency-table h4 {
        margin-top: 0;
      }

      .frequency-table table {
        width: 100%;
      }

      @media (max-width: 900px) {
        .frequency-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }

      @media (max-width: 600px) {
        .frequency-grid {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  titlePanel("CSV Data Extractor"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "csv_files",
        label = "Upload CSV files",
        multiple = TRUE,
        accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain")
      ),
      radioButtons(
        "separator",
        "Separator",
        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
        selected = ","
      ),
      selectInput(
        "percentage",
        "Percentage",
        choices = setNames(seq(10, 100, 10), paste0(seq(10, 100, 10), "%")),
        selected = 100
      )
    ),
    mainPanel(
      uiOutput("csv_tables")
    )
  )
)

server <- function(input, output, session) {
  uploaded_data <- reactive({
    req(input$csv_files)

    files <- input$csv_files
    table_ids <- paste0("csv_table_", seq_len(nrow(files)))
    toggle_ids <- paste0("show_rows_", seq_len(nrow(files)))

    lapply(seq_len(nrow(files)), function(index) {
      warning_message <- NULL

      data <- tryCatch(
        withCallingHandlers(
          extract_target_data(read_csv_grid(files$datapath[index], input$separator)),
          warning = function(warning) {
            warning_message <<- conditionMessage(warning)
            invokeRestart("muffleWarning")
          }
        ),
        error = function(error) {
          data.frame(
            Error = conditionMessage(error),
            check.names = FALSE
          )
        }
      )
      frequency_tables <- if (ncol(data) == 1 && identical(names(data), "Error")) {
        list()
      } else {
        tryCatch(
          build_frequency_tables(data, table_ids[index]),
          error = function(error) {
            list(list(
              id = paste0(table_ids[index], "_frequency_error"),
              title = "Error",
              data = data.frame(Error = conditionMessage(error), check.names = FALSE)
            ))
          }
        )
      }

      list(
        id = table_ids[index],
        toggle_id = toggle_ids[index],
        name = files$name[index],
        data = data,
        frequency_tables = frequency_tables,
        warning = warning_message
      )
    })
  })

  output$csv_tables <- renderUI({
    tables <- uploaded_data()

    tagList(lapply(tables, function(table_info) {
      tagList(
        h3(table_info$name),
        if (!is.null(table_info$warning)) {
          tags$p(table_info$warning, style = "color: #8a5a00;")
        },
        checkboxInput(table_info$toggle_id, "Show rows", FALSE),
        conditionalPanel(
          condition = sprintf("input['%s']", table_info$toggle_id),
          if (length(table_info$frequency_tables) == 0) {
            tableOutput(table_info$id)
          } else {
            tags$div(
              class = "frequency-grid",
              lapply(table_info$frequency_tables, function(frequency_table) {
                tags$div(
                  class = "frequency-table",
                  h4(frequency_table$title),
                  uiOutput(frequency_table$id)
                )
              })
            )
          }
        ),
        tags$hr()
      )
    }))
  })

  observe({
    tables <- uploaded_data()

    lapply(tables, function(table_info) {
      output[[table_info$id]] <- renderTable({
        table_info$data
      }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

      lapply(table_info$frequency_tables, function(frequency_table) {
        output[[frequency_table$id]] <- renderUI({
          req(input$percentage)
          frequency_table_html(frequency_table$data, as.numeric(input$percentage))
        })
      })
    })
  })
}

shinyApp(ui = ui, server = server)
