


#' Convert data frame to ms list object
#'
#' @param df data from experiment. Should contain metadata such as outcome, covariates, sample type (e.g., QC, sample, blind) etc.
#' @param outcome The outcome
#' @param feature_start The first column containing feature/compound values. This function assumes that no metadata columns are between this colum and the final column of feature values
#' @param feature_end The final column containing features. Default is last column in df
#' @param type If any sample type column exists input it here.
#' @param analysis_type assign the ML task (regression or classification)
#' @param tech_rep If the any column for technical replicates exits input it here.
#' @param covariates Add any potential covariates.
#'
#' @return an ms list
#' @export
#' @import dplyr tibble
#' @examples
#' library(rescience)
#' ms <- load_ms(pneumonia, outcome = "group", feature_start = 8)
load_ms <- function(df, outcome, feature_start, analysis_type = "classification", feature_end = NULL, type=NULL, batch = NULL, tech_rep=NULL, covariates = NULL){
  if(is.null(feature_end)) feature_end<-ncol(df)
  ms <- list()
  ms$values <- df[,feature_start:feature_end]
  ms$rowinfo <- df %>% dplyr::select(any_of(c(outcome, type, tech_rep, covariates, batch))) %>% tibble::rowid_to_column()
  ms$info <- list("outcome"=outcome, "feature_start"=feature_start, "analysis_type" = analysis_type, "type" = type, "batch" = batch, "tech_rep" = tech_rep, "covariates" = covariates)
  return(ms)
}

