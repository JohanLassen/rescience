#' dataloader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dataloader_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel("Upload data"),
    sidebarLayout(
      sidebarPanel(
        #h5("Select the input file (csv or tsv)"),
        fileInput(
          NS(id, "upload"),
          "Upload your data file (csv or tsv)"
          ),

        selectInput(
          NS(id, "outcome"),
          "Select outcome variable",
          "Please upload dataset"
          ),

        selectInput(
          NS(id, "batch"),
          "Select column indicating qc, sample, blank etc.",
          ""
        ),



        selectInput(
          NS(id, "tech_rep"),
          "Select column indicating technical replicates",
          ""
        ),

        numericInput(
          NS(id, "start_col"), "Features' start column", value = 1, min = 1),

        numericInput(
          NS(id, "end_col"), "Features' end column (select 0 for last column)", value=0, min = 0),

        actionButton(NS(id, "create"), "Select feature data!", class = "btn-lg btn-success")

        ),
      mainPanel(
        #h5("The first 10 columns of your data:"),
        tableOutput(NS(id, "table")),
        #h5("The first 10 selected feature values used to predict the outcome"),
        tableOutput(NS(id, "table2")),
        tableOutput(NS(id, "table3"))
      )
    )
  )
}

#' dataloader Server Functions
#'
#' @noRd
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select
mod_dataloader_server <- function(id){
  library(magrittr)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #output$

    dataset <- eventReactive(input$upload,{
        return(vroom::vroom(input$upload$datapath))
        }
    )

    tidy_data <- eventReactive(
      input$create,
      {
        df <- dataset()
        start <- input$start_col
        end   <- ifelse(input$end_col==0, ncol(df), input$end_col)

        ms         <- list()
        ms$values  <- df[,start:end]
        ms$rowinfo <- df %>% dplyr::select(any_of(c(input$outcome, input$batch, input$tech_rep)))

        ms
    })

    observe({
      f <- dataset()
      if (!is.null(f)) updateSelectInput(session, "outcome", choices=colnames(f))
    })

    observe({
      f <- dataset()
      if (!is.null(f)) updateSelectInput(session, "batch", choices=c("none", colnames(f)))
    })

    observe({
      f <- dataset()
      if (!is.null(f)) updateSelectInput(session, "tech_rep", choices=c("none", colnames(f)))
    })

    observe({
      f <- dataset()
      if (!is.null(f)) updateNumericInput(session, "start_col", max = ncol(f))
    })

    observeEvent(input$upload,
                 {
                   f <- dataset()
                   if (!is.null(f)) updateNumericInput(session, "end_col", value = ncol(f), max = ncol(f))
                   }
                 )


    output$table <- renderTable({
      return(dataset()[1:5, 1:10])
    },options = list(pageLength = 5))


    output$table2 <- renderTable({
      return(tidy_data()[[1]][1:5, 1:10])
    })

    output$table3 <- renderTable({
      return(tidy_data()[[2]][1:5, ])
    })


    list(
      outcome = reactive(input$outcome),
      batch = reactive(input$batch),
      tech_rep = reactive(input$tech_rep),
      ms = reactive(tidy_data())
    )
  })
}

## To be copied in the UI
# mod_dataloader_ui("dataloader_1")

## To be copied in the server
# mod_dataloader_server("dataloader_1")
