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


    fluidRow(
      column(
        width = 3,
        offset = 0,
        selectInput(NS(id, "transform1"),
                    p("Select transformation"),
                    c("log10", "fourth root"), selected = NULL, width = "100%"),
        actionButton(NS(id, "transform2"),"Transform"),
        br(), br(),
        selectInput(NS(id, "impute1"),
                    "Select imputation",
                    c("default", "knn"), , width = "100%"),
        actionButton(NS(id, "impute2"), "Impute", class = "btn"),
        br(), br(),
        selectInput(NS(id, "normalize1"),
                    "Select normalization",
                     c("PQN", "Rownorm", "Quantile normalization", "WaveICA2", "Combat"), , width = "100%"), # todo: "Batch-wise z-scaling", "Ratios (<100 features)"
        actionButton(NS(id, "normalize2"), "Normalize"),
        br(), br(),
        selectInput(NS(id, "outlier1"),
                    "Remove outliers?",
                    c("Yes", "No"), , width = "100%"),
        actionButton(NS(id, "outlier2"), "Confirm Outlier strategy"),
        br(), br(),
        p("
        Try avoiding using the outcome label to color the PCA plots as it increases the risk of overfitting.
        Instead see how the preprocessing adjusts for batch effect, injection order, and confounders. Read more
           ", a("here",target="_blank", href="https://johanlassen.github.io/rescience/"))


      ),
      column(
        width = 8,
        offset = 0,
        tabsetPanel(
          tabPanel("data",
                   h3("Raw"),
                   tableOutput(NS(id, "table1")),
                   uiOutput(NS(id, "transformed_tbl2")),
                   uiOutput(NS(id, "transformed_tbl3")),
                   uiOutput(NS(id, "transformed_tbl4"))
          ),
          tabPanel("PCA plots",
                   h3("Raw"),
                   plotOutput(NS(id, "plot0")),
                   uiOutput(NS(id, "transformed_pca2")),
                   uiOutput(NS(id, "transformed_pca3")),
                   uiOutput(NS(id, "transformed_pca4"))
          ),
          tabPanel("Outlier plot",
                   uiOutput(NS(id, "outlier_plot"))
                   )

        )
      )
    )
  )
}

#' preprocess Server Functions
#'
#' @noRd
mod_preprocess_server <- function(id, data){
  moduleServer( id, function(input, output, session) {
    ns <- session$ns

    ms1 <- eventReactive(input$transform2, {transform(data$ms(), input$transform1)})
    ms2 <- eventReactive(input$impute2,    {impute(ms1(), input$impute1)})
    ms3 <- eventReactive(input$normalize2, {normalize(ms2(), input$normalize1)})
    ms4 <- eventReactive(input$outlier2,    {rm_sample_pca_outliers(ms3(), plot=TRUE)})


    ### Making tables ###
    transformed_data_table2 <- eventReactive(input$transform2,{ tagList(h3("Transformed data"),tableOutput(NS(id, "table2"))) })
    output$transformed_tbl2 <- renderUI({ transformed_data_table2() })

    transformed_data_table3 <- eventReactive(input$impute2,{ tagList(h3("Imputed data"),tableOutput(NS(id, "table3"))) })
    output$transformed_tbl3 <- renderUI({ transformed_data_table3() })

    transformed_data_table4 <- eventReactive(input$normalize2,{ tagList(h3("Normalized data"),tableOutput(NS(id, "table4"))) })
    output$transformed_tbl4 <- renderUI({ transformed_data_table4() })

    ### Making PCA plots ###
    transformed_data_pca2 <- eventReactive(input$transform2,{ tagList(h3("Transformed data"),plotOutput(NS(id, "plot1"), width = "100%")) })
    output$transformed_pca2 <- renderUI({ transformed_data_pca2() })

    transformed_data_pca3 <- eventReactive(input$impute2,{ tagList(h3("Imputed data"),plotOutput(NS(id, "plot2"), width = "100%")) })
    output$transformed_pca3 <- renderUI({ transformed_data_pca3() })

    transformed_data_pca4 <- eventReactive(input$normalize2,{ tagList(h3("Normalized data"),plotOutput(NS(id, "plot3"), width = "100%")) })
    output$transformed_pca4 <- renderUI({ transformed_data_pca4() })

    ### Making outlier plot
    outlier_pca_plot <- eventReactive(input$outlier2, {
      if (input$outlier1 == "No") return()
        tagList( h3("No outliers removed. Set Remove outliers to 'yes' if you want to remove outliers"))
      if (input$outlier1 == "Yes") return(
        tagList( h3("Removed outliers"), plotOutput(NS(id, "outliers"), width = "100%"))
      )
      })
    output$outlier_plot <- renderUI({ outlier_pca_plot() })


    ### Show results of preprocessing ###
    # Tables
    output$table1 <- renderTable(data$ms()$values[1:3,1:7], width = "100%")
    output$table2 <- renderTable(ms1()$values[1:3,1:7], width = "100%")
    output$table3 <- renderTable(ms2()$values[1:3,1:7], width = "100%")
    output$table4 <- renderTable(ms3()$values[1:3,1:7], width = "100%")

    # PCA raw data
    output$plot0  <- renderPlot(plot_pca(data$ms(), color_label = c(data$batch())))
    output$plot1 <- renderPlot(plot_pca(ms1(), color_label = c(data$batch())))
    output$plot2 <- renderPlot(plot_pca(ms2(), color_label = c(data$batch())))
    output$plot3 <- renderPlot(plot_pca(ms3(), color_label = c(data$batch())))

    # PCA outliers
    output$outliers <- renderPlot(ms4()$outlier_plot)

    ### Return preprocessed ms object ###
    list(
      ms =  reactive({ms4()$ms}) # TO DO: MAKE MORE FLEXIBLE to let user do ML on raw data.´
    )
  })
}

## To be copied in the UI
# mod_preprocess_ui("preprocess_1")

## To be copied in the server
# mod_preprocess_server("preprocess_1", data_info)
