


#' Convert data frame to ms list object
#'
#' @param df data from experiment. Should contain metadata such as outcome, covariates, sample type (e.g., QC, sample, blind) etc.
#' @param outcome The outcome
#' @param feature_start The first column containing feature/compound values. This function assumes that no metadata columns are between this colum and the final column of feature values
#' @param feature_end The final column containing features. Default is last column in df
#' @param type If any sample type column exists input it here.
#' @param tech_rep If the any column for technical replicates exits input it here.
#' @param covariates Add any potential covariates.
#'
#' @return an ms list
#' @export
#' @import dplyr
#' @examples
#' df <- pneumonia
#' ms <- load_ms(pneumonia, outcome = "group", feature_start = 8)
load_ms <- function(df, outcome, feature_start, feature_end = NULL, type=NULL, tech_rep=NULL, covariates = NULL){
  if(is.null(feature_end)) feature_end<-ncol(df)
  ms <- list()
  ms$values <- df[,feature_start:feature_end]
  ms$rowinfo <- df %>% dplyr::select(any_of(c(outcome, type, tech_rep, covariates)))
  return(ms)
}


load_data <-
  function(input_value){

    file <- input_value$upload$name
    return(vroom::vroom(file))
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


# df <- vroom::vroom("../TraceAge/WP3/data/qtrap_WP2_WP3_reanalyse.csv")
# ms <- load_ms(df, outcome = "time", feature_start = 19, type = "type")
# ms <-
#   ms %>%
#   transform_log() %>%
#   impute_zero() %>%
#   normalize_limma_quantile()
#
# ms$values %>% t() %>% tibble::as_tibble() %>%  purrr::map_dbl(var) %>% min()
#
# ms %>%
#   rm_sample_pca_outliers()
# ms$values %>% View()

