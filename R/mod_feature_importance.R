#' feature_importance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_feature_importance_ui <- function(id){
  ns <- NS(id)
  tagList(

    titlePanel("Feature Importance and Post Processing"),
    sidebarLayout(
      sidebarPanel(
        selectInput(NS(id, "method1"), "Compare selected features with p-values",
                    c("Elastic Net", "Random Forest(RF)", "Gradient Boosting(GB)", "PLS(-DA)", "SVM"),  #  "OPLS(-DA)",
                    selected = "Elastic Net", multiple=T),
        actionButton(NS(id, "model2"), "Start training! (RF and GB might take +10 min to train)"),
        br(),
        actionButton(NS(id, "preprocess"), "Show the screening result (must only be done once per dataset)")

      ),
      mainPanel(
        tableOutput(NS(id, "table1")),
        plotOutput(NS(id, "plot1"))
      )
    )

  )
}

#' feature_importance Server Functions
#'
#' @noRd
mod_feature_importance_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



  })
}

## To be copied in the UI
# mod_feature_importance_ui("feature_importance_1")

## To be copied in the server
# mod_feature_importance_server("feature_importance_1")
