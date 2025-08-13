# R/mod_help.R

mod_help_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "help",
    # Section for Data Loading
    fluidRow(
      box(title = "Documentation & Tips", status = "primary", solidHeader = TRUE, width = 12,
          tags$h4("Getting Started"),
          tags$ol(
            tags$li("Upload wide format data files (Time in first column, cells in subsequent columns)"),
            tags$li("Use Data Processing to compute ΔF/F₀ if your data is raw fluorescence"),
            tags$li("View Time Course to see overall calcium dynamics"),
            tags$li("Analyze Metrics to quantify responses for each cell"),
            tags$li("Use Heatmap to visualize all cells simultaneously"),
            tags$li("Export processed data and figures for publication")
          ),
          tags$h4("Data Format"),
          tags$ul(
            tags$li("First column: Time (in seconds)"),
            tags$li("Subsequent columns: Individual cell traces"),
            tags$li("Supported formats: CSV, Excel (.xlsx, .xls)")
          )
      ) # End fluidRow
    ) # End tabItem
  ) # End tabItem
}

mod_help_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic needed for this static page
  })
}
