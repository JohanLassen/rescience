#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    navbarPage(
      "Reproducible Data Science",
      theme = bslib::bs_theme(bootswatch = "cosmo"),
      tabPanel("Data", mod_dataloader_ui("dataloader_1")),
      tabPanel("Preproces", mod_preprocess_ui("preprocess_1")),
      tabPanel("Machine Learning", mod_machine_learning_ui("machine_learning_1")),
      tabPanel("Interpretation", "To be implemented!"),
      tabPanel("Want (O)PLS?", "Get outta here!")
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
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "wwda"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
