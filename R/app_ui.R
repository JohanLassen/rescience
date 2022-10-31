#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # custom_theme <- bslib::bs_theme(
  #
  #   # # Controls the default grayscale palette
  #   # bg = "#202123", fg = "#B8BCC2",
  #   # # Controls the accent (e.g., hyperlink, button, etc) colors
  #   # primary = "#EA80FC", secondary = "#48DAC6",
  #   # base_font = c("Grandstander", "sans-serif"),
  #   # code_font = c("Courier", "monospace"),
  #   # heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  #   # # Can also add lower-level customization
  #   # "input-border-color" = "#EA80FC"
  # )
  tagList(

    # Leave this function for adding external resources
    #golem_add_external_resources(),

    # Your application UI logic
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    # ),
    navbarPage(
      #theme = ,
      "Reproducible Data Science",
      tabPanel("Data", mod_dataloader_ui("dataloader_1")),
      tabPanel("Preprocess", mod_preprocess_ui("preprocess_1")),
      tabPanel("Machine Learning", mod_machine_learning_ui("machine_learning_1")),
      tabPanel("Interpretation", p("To be implemented!")),
      tabPanel("PLS?", "Get outta here!")
      )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon()
    # bundle_resources(
    #   path = app_sys("app/www"),
    #   app_title = "Reproducible Data Science"
    # )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
