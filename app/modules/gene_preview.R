# ============================================================
# gene_preview.R - Module for gene list preview and selection
# ============================================================

genePreviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, h4("Gene List Preview",
                    style = "color: #2c3e50; font-weight: 600; margin-top: 5px;")),
      column(6, div(
        style = "text-align: right; margin-top: 5px;",
        actionGroupButtons(
          inputIds = c(ns("select_all"), ns("deselect_all")),
          labels = list(
            tags$span(icon("check-square"), " Select All"),
            tags$span(icon("square"), " Deselect All")
          ),
          size = "xs",
          status = "primary"
        )
      ))
    ),
    DTOutput(ns("preview_table")),
    br()
  )
}

genePreviewServer <- function(id, gene_lists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track selected gene lists
    selected <- reactiveVal(character(0))

    # Observe when gene_lists change
    observeEvent(gene_lists(), {
      gl <- gene_lists()
      if (length(gl) > 0) {
        selected(names(gl))
      } else {
        selected(character(0))
      }
    })

    # Select all / deselect all
    observeEvent(input$select_all, {
      gl <- gene_lists()
      if (length(gl) > 0) selected(names(gl))
    })

    observeEvent(input$deselect_all, {
      selected(character(0))
    })

    # Build the preview table
    output$preview_table <- renderDT({
      gl <- gene_lists()
      req(length(gl) > 0)

      df <- data.frame(
        Selected = names(gl) %in% selected(),
        Name = names(gl),
        `Gene Count` = sapply(gl, length),
        `First 5 Genes` = sapply(gl, function(x) {
          paste(head(x, 5), collapse = ", ")
        }),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      datatable(
        df,
        selection = "none",
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = "tip",
          columnDefs = list(
            list(
              targets = 0,
              render = JS(
                "function(data, type, row, meta) {",
                "  if (type === 'display') {",
                "    return '<input type=\"checkbox\"' + (data ? ' checked' : '') + '/>';",
                "  }",
                "  return data;",
                "}"
              ),
              className = "dt-center"
            )
          )
        ),
        callback = JS(sprintf(
          "table.on('click', 'td:first-child input[type=checkbox]', function() {
            var row = table.row($(this).closest('tr')).data();
            var name = row[1];
            Shiny.setInputValue('%s', {name: name, checked: this.checked, nonce: Math.random()});
          });",
          ns("toggle_selection")
        ))
      )
    }, server = FALSE)

    # Handle checkbox toggles
    observeEvent(input$toggle_selection, {
      info <- input$toggle_selection
      current <- selected()
      if (info$checked) {
        selected(unique(c(current, info$name)))
      } else {
        selected(setdiff(current, info$name))
      }
    })

    # Return selected gene list names
    reactive({ selected() })
  })
}
