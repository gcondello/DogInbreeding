# Packages: shiny, bslib, ggplot2, readxl, pedigreemm, pedigree
library(shiny)
library(bslib)
library(ggplot2)
library(pedigreemm)
library(pedigree)

if (file.exists("R/inbreeding.R")) {
  source("R/inbreeding.R", local = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

fmt_f <- function(x) {
  sprintf("%.4f (%.2f%%)", x, x * 100)
}

ui <- fluidPage(
  theme = bs_theme(version = 5, preset = "flatly"),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),
    tags$script(src = "sort-table.js")
  ),
  titlePanel("Dog Inbreeding Calculator"),
  div(
    class = "app-wrap",
    div(
      class = "app-sidebar well",
      h4("1. Upload pedigree"),
      fileInput(
        "file",
        NULL,
        accept = c(".csv", ".xlsx", ".xls", "text/csv"),
        placeholder = "CSV or Excel"
      ),
      helpText("One row per animal. Unknown parents can be blank, 0, or NA."),
      actionButton("load_example", "Load example pedigree", class = "btn-secondary"),
      tags$hr(),
      h4("2. Map columns"),
      selectInput("id_col", "Animal ID", choices = character(0)),
      selectInput("sire_col", "Sire", choices = character(0)),
      selectInput("dam_col", "Dam", choices = character(0)),
      helpText("Inbreeding uses only these three columns."),
      h4("3. Columns to include"),
      div(
        class = "col-actions",
        actionButton("select_all_cols", "Select all", class = "btn-secondary"),
        actionButton("clear_cols", "Clear", class = "btn-secondary")
      ),
      div(
        class = "checkbox-group-scroll",
        checkboxGroupInput("keep_cols", NULL, choices = character(0))
      ),
      helpText("Click a column name to include it in the final data output. This list will be combined with the inbreeding estimates."),
      h4("4. Estimate"),
      actionButton("estimate", "Estimate", class = "btn-primary"),
      uiOutput("validation_ui")
    ),
    div(
      class = "app-main",
      uiOutput("intro_ui"),
      h3("Summary"),
      tableOutput("summary_table"),
      h3("Distribution of inbreeding"),
      plotOutput("f_plot", height = "450px"),
      h3("Data output"),
      helpText("Sorting is optional. Click a column header to sort, click again to reverse, and click a third time to restore the original order."),
      downloadButton("download_csv", "Download CSV"),
      br(),
      br(),
      div(
        class = "data-output-wrap",
        tableOutput("results_table")
      )
    )
  )
)

server <- function(input, output, session) {
  analysis <- reactiveVal(NULL)
  upload_error <- reactiveVal(NULL)
  example_data <- reactiveVal(NULL)
  available_cols <- reactiveVal(character(0))

  uploaded <- reactive({
    if (!is.null(example_data())) {
      return(example_data())
    }
    req(input$file)
    tryCatch(
      read_pedigree_file(input$file$datapath, input$file$name),
      error = function(e) {
        upload_error(e$message)
        NULL
      }
    )
  })

  observeEvent(input$load_example, {
    path <- file.path("data", "example_pedigree.csv")
    if (!file.exists(path)) {
      upload_error("Could not find data/example_pedigree.csv.")
      return()
    }
    example_data(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
    analysis(NULL)
    upload_error(NULL)
    cols <- names(example_data())
    available_cols(cols)
    guessed <- guess_pedigree_columns(cols)
    updateSelectInput(session, "id_col", choices = cols, selected = guessed$id)
    updateSelectInput(session, "sire_col", choices = cols, selected = guessed$sire)
    updateSelectInput(session, "dam_col", choices = cols, selected = guessed$dam)
    updateCheckboxGroupInput(
      session,
      "keep_cols",
      choices = cols,
      selected = default_keep_cols(cols, guessed)
    )
  })

  observeEvent(input$file, {
    example_data(NULL)
    analysis(NULL)
    upload_error(NULL)
    df <- uploaded()
    if (is.null(df) || length(names(df)) == 0) {
      available_cols(character(0))
      updateSelectInput(session, "id_col", choices = character(0))
      updateSelectInput(session, "sire_col", choices = character(0))
      updateSelectInput(session, "dam_col", choices = character(0))
      updateCheckboxGroupInput(session, "keep_cols", choices = character(0))
      return()
    }
    cols <- names(df)
    available_cols(cols)
    guessed <- guess_pedigree_columns(cols)
    updateSelectInput(session, "id_col", choices = cols, selected = guessed$id)
    updateSelectInput(session, "sire_col", choices = cols, selected = guessed$sire)
    updateSelectInput(session, "dam_col", choices = cols, selected = guessed$dam)
    updateCheckboxGroupInput(
      session,
      "keep_cols",
      choices = cols,
      selected = default_keep_cols(cols, guessed)
    )
  })

  observeEvent(input$select_all_cols, {
    cols <- available_cols()
    updateCheckboxGroupInput(session, "keep_cols", choices = cols, selected = cols)
  })

  observeEvent(input$clear_cols, {
    cols <- available_cols()
    updateCheckboxGroupInput(session, "keep_cols", choices = cols, selected = character(0))
  })

  observeEvent(input$estimate, {
    if (is.null(input$file) && is.null(example_data())) {
      analysis(list(
        ok = FALSE,
        errors = "Upload a pedigree file first.",
        warnings = character(),
        results = NULL
      ))
      return()
    }
    df <- uploaded()
    if (is.null(df)) {
      analysis(list(
        ok = FALSE,
        errors = upload_error() %||% "Could not read the uploaded file.",
        warnings = character(),
        results = NULL
      ))
      return()
    }
    result <- run_inbreeding(df, input$id_col, input$sire_col, input$dam_col)
    if (isTRUE(result$ok)) {
      result$source <- df
      result$id_col <- input$id_col
    }
    analysis(result)
  })

  output$validation_ui <- renderUI({
    if (!is.null(upload_error()) && is.null(analysis())) {
      return(div(class = "alert alert-danger", upload_error()))
    }
    out <- analysis()
    req(out)
    msgs <- list()
    if (length(out$errors) > 0) {
      msgs <- c(msgs, list(div(
        class = "alert alert-danger",
        tags$strong("Could not calculate"),
        tags$ul(lapply(out$errors, tags$li))
      )))
    }
    if (length(out$warnings) > 0) {
      msgs <- c(msgs, list(div(
        class = "alert alert-warning",
        tags$strong("Warning"),
        tags$ul(lapply(out$warnings, tags$li))
      )))
    }
    if (isTRUE(out$ok)) {
      msgs <- c(msgs, list(div(
        class = "alert alert-success",
        "Data output is ready."
      )))
    }
    tagList(msgs)
  })

  output$intro_ui <- renderUI({
    out <- analysis()
    if (!is.null(out) && isTRUE(out$ok)) {
      return(NULL)
    }
    wellPanel(
      tags$p("Upload a CSV or Excel pedigree, check the ID / sire / dam columns, choose any extra columns to keep, then click Calculate."),
      tags$p("Estimates are Wright's inbreeding coefficient F (0–1), also shown as a percentage. Founders with unknown parents receive F = 0.")
    )
  })

  results_df <- reactive({
    out <- analysis()
    req(isTRUE(out$ok), !is.null(out$results))
    out$results
  })

  output$summary_table <- renderTable({
    df <- results_df()
    data.frame(
      Animals = nrow(df),
      `Mean F` = fmt_f(mean(df$inbreeding, na.rm = TRUE)),
      `Median F` = fmt_f(stats::median(df$inbreeding, na.rm = TRUE)),
      `Max F` = fmt_f(max(df$inbreeding, na.rm = TRUE)),
      check.names = FALSE
    )
  })

  output$f_plot <- renderPlot({
    df <- results_df()
    ggplot(df, aes(x = inbreeding)) +
      geom_histogram(bins = 20, fill = "#2c3e50", color = "white") +
      labs(
        x = "Inbreeding coefficient (F)",
        y = "Number of animals"
      ) +
      theme_minimal(base_size = 14)
  })

  output$results_table <- renderTable({
    df <- export_df()
    if ("inbreeding" %in% names(df)) {
      df$inbreeding <- sprintf("%.4f", df$inbreeding)
    }
    if ("inbreeding_pct" %in% names(df)) {
      df$inbreeding_pct <- sprintf("%.2f", df$inbreeding_pct)
    }
    df
  })

  export_df <- reactive({
    out_analysis <- analysis()
    results <- results_df()
    src <- out_analysis$source
    id_col <- out_analysis$id_col
    req(src, id_col, id_col %in% names(src))

    ids <- normalize_id(src[[id_col]])
    f_by_id <- results[, c("id", "inbreeding", "inbreeding_pct"), drop = FALSE]
    inbreeding <- f_by_id$inbreeding[match(ids, f_by_id$id)]
    inbreeding_pct <- f_by_id$inbreeding_pct[match(ids, f_by_id$id)]

    keep_cols <- input$keep_cols
    keep_cols <- keep_cols[keep_cols %in% names(src)]
    keep_cols <- setdiff(keep_cols, c("inbreeding", "inbreeding_pct"))
    if (length(keep_cols) == 0) {
      keep_cols <- id_col
    }

    exported <- src[, keep_cols, drop = FALSE]
    exported$inbreeding <- inbreeding
    exported$inbreeding_pct <- inbreeding_pct
    exported
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("inbreeding_estimates_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(export_df(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
