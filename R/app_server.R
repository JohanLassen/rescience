#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  options(shiny.maxRequestSize=100*1024^2)
  data         <- mod_dataloader_server("dataloader_1")
  preprocessed <- mod_preprocess_server("preprocess_1", data)
  fits         <- mod_machine_learning_server("machine_learning_1", preprocessed, data)

}
