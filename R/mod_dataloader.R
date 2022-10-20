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
    tags$style(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      )),
    titlePanel("Upload data"),

    fluidRow(
    column(
        width = 3,
        offset = 0,

        fileInput(
          NS(id, "upload"),
          "Upload your data file (csv or tsv)",
          width = "100%"
        ),

        selectInput(
          NS(id, "outcome"),
          "Select outcome variable",
          "Please upload dataset",
          width = "100%"
        ),

        selectInput(
          NS(id, "batch"),
          "Select column indicating qc, sample, blank etc.",
          "",
          width = "100%"
        ),

        selectInput(
          NS(id, "tech_rep"),
          "Select column indicating technical replicates",
          "",
          width = "100%"
        ),

        numericInput(
          NS(id, "start_col"), "Features' start column", value = 1, min = 1,
          width = "100%"),

        numericInput(
          NS(id, "end_col"), "Features' end column (select 0 for last column)", value=0, min = 0,
          width = "100%"),

        actionButton(NS(id, "create"), "Select feature data!")

     ),

      column(
        width = 3,
        offset = 1,
        fluidRow(
          uiOutput(NS(id, "uploaded")),
          # h3("Imported data"),
          # tableOutput(NS(id, "table")),
          uiOutput(NS(id, "setup"))
          # h3("Data used for modeling", width = "100%"),
          # column(
          #   width = 3,
          #   offset = 0,
          #   h5("Meta data"),
          #   tableOutput(NS(id, "table3"))
          #   ),
          # column(
          #   width = 4,
          #   offset = 3,
          #   h5("Values"),
          #   tableOutput(NS(id, "table2"))
          #   )
        )
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
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Load data
    dataset <- eventReactive(input$upload,{
        return(vroom::vroom(input$upload$datapath))
        }
    )
    output$fileUploaded <- reactive({
      return(!is.null(input$upload))
    })

    # Show uploaded data table
    uploaded_data <- eventReactive(input$upload,{tagList(h3("Imported data"),tableOutput(NS(id, "table")))})
    output$uploaded <- renderUI({ uploaded_data() })

    # Show data selected for modeling
    ready_data <- eventReactive(input$create, {
      tagList(
        h3("Data used for modeling", width = "100%"),
        fluidRow(
          column(
            width = 3,
            offset = 0,
            h5("Meta data"),
            tableOutput(NS(id, "table3"))
          ),
          column(
            width = 4,
            offset = 3,
            h5("Values"),
            tableOutput(NS(id, "table2"))
          )
        )
      )
    })
    output$setup <- renderUI({ ready_data() })

    # Show

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
      if (!is.null(f)) updateSelectInput(session, "outcome", choices=colnames(f)[1:50])
    })

    observe({
      f <- dataset()
      if (!is.null(f)) updateSelectInput(session, "batch", choices=c("none", colnames(f)[1:50]))
    })

    observe({
      f <- dataset()
      if (!is.null(f)) updateSelectInput(session, "tech_rep", choices=c("none", colnames(f)[1:50]))
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
    },options = list(pageLength = 5), width = "100%")


    output$table2 <- renderTable({
      return(tidy_data()[[1]][1:5, 1:6])
    }, width = "100%")

    output$table3 <- renderTable({
      return(tidy_data()[[2]][1:5, ])
    }, width = "100%")


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
