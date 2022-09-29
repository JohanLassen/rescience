#' machine_learning UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_machine_learning_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel("Machine Learning"),
    sidebarLayout(
      sidebarPanel(
        selectInput(NS(id, "model1"), "Select multiple models for model screening",
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


#' machine_learning Server Functions
#'
#' @noRd
mod_machine_learning_server <- function(id, preprocessed, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    models <- eventReactive(
      input$model2,
      {
        print("hej")
        #preprocessed$ms()$values[1:10, 1:10]
        model_screening(preprocessed$ms(), methods = input$model1, outcome = data$outcome())
      })

    performance_plot <-
      eventReactive(input$model2,
                    {plot_performance(models())})

   # output$table1 <- renderTable(models())
    output$plot1 <- renderPlot(performance_plot())

    list(
      fits = reactive(models())
    )

  })
}

## To be copied in the UI
# mod_machine_learning_ui("machine_learning_1")

## To be copied in the server
# mod_machine_learning_server("machine_learning_1")
