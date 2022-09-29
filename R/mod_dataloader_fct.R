



load_data <-
  function(input_value){

    file <- input_value$upload$name
    return(vroom::vroom(file))

    # if (is.null(input_value$upload)){return()}
    # if (grepl("[.]csv", file)){
    #   return(readr::read_csv(input_value$upload$datapath))
    # } else {
    #   return(readr::read_tsv(input_value$upload$datapath))
    # }
  }


preprocess <-
  function()
    eventReactive(
      input$select_columns, {
        df <- dataset()
        y  <- df[[input$y_pos]]
        x_end <- ifelse(input$x_end == 0, ncol(df), input$x_end)
        x <- df[input$x_start:x_end]
        x <- as.matrix(x)^1/4
        x <- pqn(x) %>% scale()
        return(list("x"=x, "y"=y))
      })
