# ============================================================
# data_loader.R - Module for loading gene list data
# ============================================================

# --- Helper functions ---

#' Check if the first element looks like a header
is_header <- function(value, patterns = HEADER_PATTERNS) {
  if (is.na(value) || is.null(value)) return(TRUE)
  value_clean <- trimws(tolower(as.character(value)))
  if (value_clean == "" || value_clean == "na") return(TRUE)
  any(sapply(patterns, function(p) grepl(p, value_clean, ignore.case = TRUE)))
}

#' Process a single gene vector: check header, remove duplicates, return clean list
process_gene_vector <- function(genes, list_name) {
  warnings <- character(0)

  # Remove empty strings and NAs
  genes <- genes[!is.na(genes) & trimws(genes) != ""]
  genes <- trimws(as.character(genes))

  if (length(genes) == 0) {
    return(list(genes = character(0), warnings = warnings))
  }

  # Check if first element is a header
  if (is_header(genes[1])) {
    warnings <- c(warnings, paste0(
      "Header detected in '", list_name, "': '", genes[1],
      "' was removed."
    ))
    genes <- genes[-1]
  }

  # Check for duplicates
  dups <- duplicated(genes)
  if (any(dups)) {
    dup_genes <- unique(genes[dups])
    n_dups <- sum(dups)
    warnings <- c(warnings, paste0(
      "Found ", n_dups, " duplicate gene(s) in '", list_name,
      "' (e.g., ", paste(head(dup_genes, 3), collapse = ", "),
      "). Keeping first occurrence only."
    ))
    genes <- genes[!dups]
  }

  list(genes = genes, warnings = warnings)
}

#' Make names unique by appending a number to duplicates
make_unique_names <- function(names_vec) {
  counts <- table(names_vec)
  dups <- names(counts[counts > 1])
  for (d in dups) {
    idx <- which(names_vec == d)
    names_vec[idx] <- paste0(d, "_", seq_along(idx))
  }
  names_vec
}

# --- UI module ---

dataLoaderUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Load Gene Lists", style = "margin-top: 0; color: #ecf0f1; font-weight: 600;"),
    hr(style = "border-color: #4a6785;"),

    radioGroupButtons(
      inputId = ns("input_type"),
      label = "Input Type:",
      choices = c(
        "Folder (txt/csv)" = "folder",
        "Excel (.xlsx)" = "excel",
        "RDS (.rds)" = "rds"
      ),
      direction = "vertical",
      justified = TRUE,
      size = "sm",
      status = "primary"
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'folder'", ns("input_type")),
      fileInput(
        ns("folder_files"), "Upload txt/csv files:",
        multiple = TRUE,
        accept = c(".txt", ".csv"),
        placeholder = "Select files..."
      )
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'excel'", ns("input_type")),
      fileInput(
        ns("excel_file"), "Upload Excel file:",
        multiple = FALSE,
        accept = c(".xlsx", ".xls"),
        placeholder = "Select .xlsx file..."
      )
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'rds'", ns("input_type")),
      fileInput(
        ns("rds_file"), "Upload RDS file:",
        multiple = FALSE,
        accept = c(".rds"),
        placeholder = "Select .rds file..."
      )
    ),

    hr(style = "border-color: #4a6785;"),
    uiOutput(ns("warnings_ui"))
  )
}

# --- Server module ---

dataLoaderServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive: load gene lists
    gene_data <- reactive({
      gene_lists <- list()
      all_warnings <- character(0)

      # --- Folder of txt/csv files ---
      if (input$input_type == "folder" && !is.null(input$folder_files)) {
        files <- input$folder_files
        names_vec <- tools::file_path_sans_ext(files$name)
        names_vec <- make_unique_names(names_vec)

        for (i in seq_len(nrow(files))) {
          ext <- tolower(tools::file_ext(files$name[i]))
          tryCatch({
            if (ext == "csv") {
              df <- read.csv(files$datapath[i], header = FALSE,
                             stringsAsFactors = FALSE, check.names = FALSE)
            } else {
              df <- read.delim(files$datapath[i], header = FALSE,
                               stringsAsFactors = FALSE, check.names = FALSE)
            }
            genes <- as.character(df[[1]])
            result <- process_gene_vector(genes, names_vec[i])
            gene_lists[[names_vec[i]]] <- result$genes
            all_warnings <- c(all_warnings, result$warnings)
          }, error = function(e) {
            all_warnings <<- c(all_warnings, paste0(
              "Error reading '", files$name[i], "': ", e$message
            ))
          })
        }
      }

      # --- Excel file ---
      if (input$input_type == "excel" && !is.null(input$excel_file)) {
        file_path <- input$excel_file$datapath
        tryCatch({
          sheets <- excel_sheets(file_path)
          sheets <- make_unique_names(sheets)
          for (i in seq_along(sheets)) {
            df <- read_excel(file_path, sheet = i, col_names = FALSE)
            if (ncol(df) > 0) {
              genes <- as.character(df[[1]])
              result <- process_gene_vector(genes, sheets[i])
              gene_lists[[sheets[i]]] <- result$genes
              all_warnings <- c(all_warnings, result$warnings)
            }
          }
        }, error = function(e) {
          all_warnings <<- c(all_warnings, paste0(
            "Error reading Excel file: ", e$message
          ))
        })
      }

      # --- RDS file ---
      if (input$input_type == "rds" && !is.null(input$rds_file)) {
        file_path <- input$rds_file$datapath
        tryCatch({
          data <- readRDS(file_path)
          if (!is.list(data)) {
            all_warnings <- c(all_warnings,
              "RDS file does not contain a named list of gene vectors.")
          } else {
            names_vec <- names(data)
            if (is.null(names_vec)) {
              names_vec <- paste0("List_", seq_along(data))
            }
            names_vec <- make_unique_names(names_vec)
            for (i in seq_along(data)) {
              genes <- as.character(data[[i]])
              result <- process_gene_vector(genes, names_vec[i])
              gene_lists[[names_vec[i]]] <- result$genes
              all_warnings <- c(all_warnings, result$warnings)
            }
          }
        }, error = function(e) {
          all_warnings <<- c(all_warnings, paste0(
            "Error reading RDS file: ", e$message
          ))
        })
      }

      list(gene_lists = gene_lists, warnings = all_warnings)
    })

    # Display warnings
    output$warnings_ui <- renderUI({
      w <- gene_data()$warnings
      if (length(w) == 0) return(NULL)
      tagList(
        lapply(w, function(msg) {
          div(
            class = "alert alert-warning",
            style = "padding: 8px 12px; margin-bottom: 6px; font-size: 12px; border-radius: 4px; background-color: #f39c12; color: #fff; border: none;",
            icon("exclamation-triangle"), " ", msg
          )
        })
      )
    })

    # Return reactive gene lists
    reactive({ gene_data()$gene_lists })
  })
}
