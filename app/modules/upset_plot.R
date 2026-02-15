# ============================================================
# upset_plot.R - Module for UpSet plot visualization
# ============================================================

upsetPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("UpSet Plot", style = "color: #2c3e50; font-weight: 600;"),
    fluidRow(
      column(8,
        plotOutput(ns("upset_plot"), height = "auto"),
        # Download section embedded in the plot tab
        hr(style = "border-color: #e9ecef;"),
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
          fluidRow(
            column(4, h5(icon("download"), " Download UpSet Plot",
                         style = "font-weight: 600; color: #2c3e50; margin-top: 5px;")),
            column(3, selectInput(ns("fig_format"), NULL,
                        choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg"),
                        width = "100%")),
            column(3, downloadButton(ns("download_figure"), "Download",
                        class = "btn-primary", style = "width: 100%; margin-top: 1px;")),
            column(2)
          )
        )
      ),
      column(4,
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
          h5("Plot Options", style = "color: #2c3e50; font-weight: 600; margin-top: 0;"),
          textInput(ns("title"), "Title:", value = ""),
          sliderInput(ns("width"), "Width (px):", 400, 1200, 700, step = 50),
          sliderInput(ns("height"), "Height (px):", 300, 1000, 500, step = 50),
          sliderInput(ns("nintersects"), "Max Intersections:", 5, 60, 40, step = 5),
          sliderInput(ns("point_size"), "Point Size:", 1, 8, 3, step = 0.5),
          sliderInput(ns("line_width"), "Line Width:", 0.5, 4, 1.5, step = 0.5),
          sliderInput(ns("text_scale"), "Text Scale:", 0.5, 3.0, 1.3, step = 0.1),
          sliderInput(ns("mb_ratio"), "Matrix/Bar Ratio:", 0.2, 0.8, 0.55, step = 0.05),
          hr(style = "border-color: #dee2e6;"),
          h6("Colors", style = "font-weight: 600; color: #2c3e50;"),
          selectInput(ns("palette"), "Color Palette:",
                       choices = names(COLOR_PALETTES),
                       selected = "Tableau 10"),
          colourInput(ns("main_color"), "Intersection Bar Color:", value = "#4E79A7",
                      showColour = "background", palette = "limited"),
          colourInput(ns("matrix_color"), "Matrix Point Color:", value = "#2c3e50",
                      showColour = "background", palette = "limited"),
          hr(style = "border-color: #dee2e6;"),
          selectInput(ns("order_by"), "Order By:",
                      choices = c("Frequency" = "freq", "Degree" = "degree"),
                      selected = "freq"),
          checkboxInput(ns("show_numbers"), "Show Intersection Sizes", value = TRUE),
          checkboxInput(ns("decreasing"), "Decreasing Order", value = TRUE)
        )
      )
    )
  )
}

upsetPlotServer <- function(id, gene_lists, selected_names) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Build the selected gene lists
    selected_lists <- reactive({
      gl <- gene_lists()
      sel <- selected_names()
      req(length(sel) >= 2)
      gl[sel]
    })

    # Get the set bar colors from the selected palette (one distinct color per set)
    set_colors <- reactive({
      sl <- selected_lists()
      pal <- COLOR_PALETTES[[input$palette]]
      n <- length(sl)
      # Recycle palette if more sets than colors
      rep_len(pal, n)
    })

    # Helper to create the upset plot object
    make_upset <- function(sl) {
      cols <- set_colors()
      upset(
        fromList(sl),
        nsets = length(sl),
        nintersects = input$nintersects,
        order.by = input$order_by,
        decreasing = c(input$decreasing, FALSE),
        show.numbers = ifelse(input$show_numbers, "yes", "no"),
        point.size = input$point_size,
        line.size = input$line_width,
        mainbar.y.label = "Intersection Size",
        sets.x.label = "Set Size",
        text.scale = rep(input$text_scale, 6),
        mb.ratio = c(input$mb_ratio, 1 - input$mb_ratio),
        main.bar.color = input$main_color,
        matrix.color = input$matrix_color,
        sets.bar.color = cols
      )
    }

    # Render the UpSet plot — must use print() for upset objects
    output$upset_plot <- renderPlot({
      sl <- selected_lists()
      req(length(sl) >= 2)

      p <- make_upset(sl)
      print(p)

      # Add title if provided
      if (nchar(trimws(input$title)) > 0) {
        grid.text(
          input$title,
          x = 0.65, y = 0.95,
          gp = gpar(fontsize = 16, fontface = "bold", col = "#2c3e50")
        )
      }

    }, width = function() input$width,
       height = function() input$height)

    # --- Download handler for UpSet figure ---
    output$download_figure <- downloadHandler(
      filename = function() {
        paste0("upset_plot.", input$fig_format)
      },
      content = function(file) {
        w <- input$width
        h <- input$height

        if (input$fig_format == "pdf") {
          pdf(file, width = w / 100, height = h / 100)
        } else if (input$fig_format == "png") {
          png(file, width = w, height = h, res = 150)
        } else {
          jpeg(file, width = w, height = h, quality = 95, res = 150)
        }

        tryCatch({
          sl <- selected_lists()
          print(make_upset(sl))
          if (nchar(trimws(input$title)) > 0) {
            grid.text(
              input$title,
              x = 0.65, y = 0.95,
              gp = gpar(fontsize = 16, fontface = "bold", col = "#2c3e50")
            )
          }
        }, finally = {
          dev.off()
        })
      }
    )

    # Return the plot info for external use (kept for compatibility)
    reactive({
      list(
        plot_fn = function() {
          sl <- selected_lists()
          print(make_upset(sl))
          if (nchar(trimws(input$title)) > 0) {
            grid.text(
              input$title,
              x = 0.65, y = 0.95,
              gp = gpar(fontsize = 16, fontface = "bold", col = "#2c3e50")
            )
          }
        },
        width = input$width,
        height = input$height
      )
    })
  })
}
