#' preprocess UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_preprocess_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel("Preprocess selected data"),
    # setBackgroundImage(
    #   src = "https://www.fillmurray.com/1920/1080"
    # ),
    sidebarLayout(
      sidebarPanel(
        selectInput(NS(id, "transform1"), "Select transformation to make data normally distributed", c("log10", "fourth root"), selected = NULL),
        actionButton(NS(id, "transform2"), "Transform data"),
        selectInput(NS(id, "impute1"), "Impute? (default imputation: 0)", c("default", "knn")),
        actionButton(NS(id, "impute2"), "Impute data"),
        selectInput(NS(id, "normalize1"), "Select the normalization you believe is best",
                     c("PQN", "Rownorm", "Quantile normalization", "WaveICA2", "Combat")), # todo: "Batch-wise z-scaling", "Ratios (<100 features)"
        actionButton(NS(id, "normalize2"), "Normalize data"),
        selectInput(NS(id, "outlier"), "Remove outliers? (by PCA measure)", c("Yes!", "No!"))
        # radioButtons(NS(id, "order"), "I want to remove outliers before I normalize", c("Yes", "No"), selected = "No"),
        # h5("
        # We show the results of the preprocessing as PCA plots colored by batch.
        # You can try to run additional combinations to optimize
        # the unsupervized learning but the moment you choose to
        # see how it works on the machine learning you should freeze
        # your preprocessing setup. Else you will overfit and your
        # results wont be as reproducible! Read more on :::
        #    "),

      ),
      mainPanel(
        tableOutput(NS(id, "table1")),
        tableOutput(NS(id, "table2")),
        tableOutput(NS(id, "table3")),
        tableOutput(NS(id, "table4")),
        plotOutput(NS(id, "plot1"), width = "1000px"),
        plotOutput(NS(id, "plot2"), width = "1000px"),
        plotOutput(NS(id, "plot3"), width = "1000px")
      )
    )
  )
}

#' preprocess Server Functions
#'
#' @noRd
mod_preprocess_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ms1 <- eventReactive(input$transform2, {transform(data$ms(), input$transform1)})
    ms2 <- eventReactive(input$impute2,    {impute(ms1(), input$impute1)})
    ms3 <- eventReactive(input$normalize2, {normalize(ms2(), input$normalize1)})

    output$table1 <- renderTable(head(data$ms()$values[,1:10]), width = "400px")
    output$table2 <- renderTable(head(ms1()$values[,1:10]), width = "400px")
    output$table3 <- renderTable(head(ms2()$values[,1:10]), width = "400px")
    output$table4 <- renderTable(head(ms3()$values[,1:10]), width = "400px")
    output$plot1 <- renderPlot(plot_pca(ms1(), color_labels = c(data$batch())))
    output$plot2 <- renderPlot(plot_pca(ms2(), color_labels = c(data$batch())))
    output$plot3 <- renderPlot(plot_pca(ms3(), color_labels = c(data$batch())))


    list(ms = reactive(ms3()))

  })
}


## To be copied in the UI
# mod_preprocess_ui("preprocess_1")

## To be copied in the server
# mod_preprocess_server("preprocess_1", data_info)
