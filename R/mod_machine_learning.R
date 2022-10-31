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
    fluidRow(
      column(
        width = 3,
        offset = 0,
        selectInput(NS(id, "model1"), "Select multiple models for model screening",
                    c("Elastic Net", "Random Forest(RF)", "Gradient Boosting(GB)", "PLS(-DA)", "SVM"),  #  "OPLS(-DA)",
                    selected = "Elastic Net", multiple=T, width = "100%"),
        actionButton(NS(id, "model2"), "Start training!", width = "100%"),
        p("Note that random forest and gradient boosting may take +10 minutes.
          Start out using elastic net and PLS to see if it works.", width = "100%"),
        br(),br(),
        actionButton(NS(id, "preprocess"), "Final performance", width = "100%"),
        p("In theory you should only press this once to avoid overfitting", width = "100%")
      ),

      # Show tables of imported data and data ready for preprocessing
      column(
        width = 4,
        offset = 0,
        plotOutput(NS(id, "plot1"), width = "100%"),
        plotOutput(NS(id, "plot3"), width = "100%")
        ),
        column(
          width = 4,
          offset = 0,
          plotOutput(NS(id, "plot2"), width = "100%")
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

    # dev_data <- reactive({
    #   df <- rescience::pneumonia
    #   ms <- load_ms(df, outcome = "group", feature_start = 8, type = "id")
    #   ms <- ms %>% transform_log() %>% impute_zero() %>% normalize_pqn()
    #   ms
    # })

    models <- eventReactive(
      input$model2,
      {
        model_screening(ms = preprocessed$ms(), outcome = data$outcome(), analysis_type = data$analysis_type(), methods = input$model1)
        #model_screening(ms = dev_data(), outcome = "group", analysis_type = "Classification", methods = input$model1)

      })

   # output$table1 <- renderTable(models())
    output$plot1 <- renderPlot(plot_roc(models()))
    output$plot2 <- renderPlot(plot_metrics(models()))
    output$plot3 <- renderPlot(probs_hist(models()))
    list(
      fits = reactive(models())
    )

  })
}

## To be copied in the UI
# mod_machine_learning_ui("machine_learning_1")

## To be copied in the server
# mod_machine_learning_server("machine_learning_1")
