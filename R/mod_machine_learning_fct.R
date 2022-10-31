

#' Model screening
#'
#' @param ms ms list object containing value and meta data
#' @param methods machine learning methods to fit
#' @param outcome outcome variable to predict
#' @param tech_rep makes sure technical replicates end up together in hold out data to prevent data leakage
#'
#' @return list of caret models
#'
model_screening <-
  function(ms, methods, outcome, analysis_type, tech_rep = NULL){
    models <- list("Elastic Net" = "glmnet", "Random Forest(RF)"="ranger", "Gradient Boosting(GB)"="xgbTree", "PLS(-DA)" = "pls", "SVM"="svm")
    for (i in 1:length(methods)){
      methods[i] <- models[[methods[i]]]
    }

    fits <- fit_models(ms=ms, outcome=outcome,  methods=methods, analysis_type=analysis_type)
    return(fits)
  }


#' Matthews Correlation Coefficient
#'
#' The mcc value is robust to skewed outcome distributions and always represent the amount of information extracted by the model.
#'
#' @param pred predictions from any model
#' @param obs observed outcome values
#'
#' @return mcc score
mcc <-
  function(pred, obs){
    cm  <- table(pred, obs) / 100 #confusion matrix
    TP  <- cm[1,1]
    FN  <- cm[1,2]
    FP  <- cm[2,1]
    TN  <- cm[2,2]
    mcc <- (TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    return(mcc)
  }

# TODO: Make fit functions for small scale, med scale, and large scale data: [:50]=LOOCV, [50:200]=RepCV, [200:]= repCV and validation (true hold out)


#' Fit models
#' Use several different model types to predict on the x and y
#' @param x value data (matrix, df, or tibble)
#' @param y outcome variable (vector)
#' @param methods machine learning models
#' @import caret glmnet ranger
#' @return list of caret models
#' @export
fit_models <- function(ms, outcome, analysis_type="Classification", methods = c("glmnet", "pls")){

  x <- as.matrix(ms$values)
  y <- ms$rowinfo[[outcome]]
  if (analysis_type=="Classification") {
    y <- as.factor(y)
  } else{
    y <- as.numeric(y)
  }

  cat("\nNotice:\nDepending on the data size this process may take 1-60 minutes\n\n")
  trctrl <- caret::trainControl(method = "repeatedcv", number = 5, repeats = 3, verboseIter = T, savePredictions = T, classProbs = T)
  fits   <-
    as.list(methods) %>%
    purrr::map(~do.call(
      caret::train,
      c(
        list(x = x, y=y, method = .x, tuneLength = 3, trControl = trctrl),
        list(importance = 'impurity')[.x=="ranger"]
      ))
    )
  names(fits) <- methods
  return(fits)
}



#' Extracts performance from list of caret models
#'
#' @param fits list of caret models
#'
#' @return data frame of accuracy and mcc (classification metrics)
#' @export
#'
get_performance <-
  function(fits){
    print(fits)
    predictions <-
      fits %>%
      purrr::map(5) %>%
      dplyr::bind_rows(.id = "method") %>%
      dplyr::mutate(
        rep = gsub(".*[.]", "", Resample),
        fold = gsub("[.].*", "", Resample)
      )

    performance <-
      predictions %>%
      dplyr::group_by(method, rep) %>%
      dplyr::summarise(accuracy = mean(pred == obs), mcc = mcc(pred, obs))

    return(performance)
  }




#' ROC calculations
#'
#' @param pred_object caret model output pred element
#'
#' @return tibble with roc data
get_roc <- function(pred_object, classes){
  pred_object %>%
      mutate(
        probs = pred_probs,
        actuals = (obs==classes[2])[order(probs)],
        threshold = probs[order(probs)],
        sens = (sum(actuals) - cumsum(actuals))/sum(actuals),
        spec = cumsum(!actuals)/sum(!actuals),
        FPR  = 1-spec,
        AUC  = sum(spec*diff(c(0, 1 - sens)))
      ) %>%
      select(TPR = sens, FPR, AUC, p = threshold)
}



#' Plot performance of screened ML models
#'
#' @param fits list of caret models
#'
#' @return ROC plot and repeat-wise performance plot
#'
#' @import MLeval
#' @export
#'
plot_roc <-
  function(fits){

    if (fits[[1]]$modelType == "Classification"){
      classes <- as.character(fits[[1]]$levels)
      pos_class <- classes[2]
    } else {stop("The inputted model is not a classification model")}

    repeat_rocs <-
      fits %>%
      purrr::map("pred") %>%
      purrr::map(dplyr::mutate, repeats = gsub(".*Rep", "", Resample), pred_probs = .data[[classes[2]]]) %>%
      purrr::map(~{
        .x %>%
          dplyr::group_split(repeats) %>%
          purrr::map(get_roc, classes = classes) %>%
          bind_rows(.id = "repeats")
      }) %>% dplyr::bind_rows(.id = "model")

    mean_rocs <-
      fits %>%
      purrr::map("pred") %>% #pull out preds from caret models
      dplyr::bind_rows(.id = "model") %>%  # merge preds, make model column
      dplyr::mutate(resample = gsub("Fold|Rep.*", "", Resample), pred_probs = .data[[classes[2]]]) %>% # convert class probs columns to pred_probs, make repeats columns
      dplyr::group_by(model, resample, rowIndex) %>%  # for each repeat, model, and sample do:
      dplyr::summarise(obs = obs[1], pred_probs = mean(pred_probs)) %>% dplyr::ungroup() %>%  #calculate mean probability across folds
      dplyr::group_split(model) %>% # Split back into models before...
      purrr::map(get_roc, classes = classes) #... calculating rocs for each model

    names(mean_rocs) <- names(fits)
    mean_rocs <- mean_rocs %>% dplyr::bind_rows(.id = "model")

    AUC_metrics <- unique(mean_rocs[,c("model", "AUC")]) %>% mutate(label = paste0(model, ": ", round(AUC,2))) %>%  dplyr::mutate(x=rep(0.8, length(fits)), y=0.06+0.06*seq(length(fits)))

    roc_curve <-
      ggplot2::ggplot()+
      ggplot2::geom_segment(ggplot2::aes(x=0, y=0, xend=1, yend=1), color="gray70", linetype="dashed")+
      ggplot2::geom_line(data = mean_rocs, ggplot2::aes(x=FPR, y=TPR, color = model), show.legend = F, size=1.1)+
      ggplot2::geom_line(data = repeat_rocs, ggplot2::aes(x=FPR, y=TPR, color = model, groups = repeats), show.legend = F, size=1.1, alpha = 0.2)+
      ggplot2::scale_colour_hue(l = 45) +
      ggplot2::theme_minimal()+
      ggplot2::theme(legend.position = c(0.9, 0.9))+
      ggplot2::geom_text(data=AUC_metrics, ggplot2::aes(x=x, y=y, label=label, color = model), show.legend = F, hjust=0)+
      ggplot2::labs(x="false postive rate", y="true positive rate", title = "AUC-ROC", subtitle = "Background: Repeats from cross validation\nForeground: Mean predictions of repeats")

    return(roc_curve)
  }


plot_metrics <-
  function(fits){

    performance <-
      get_performance(fits)

    metrics_plot <-
      performance %>%
      ggplot2::ggplot(ggplot2::aes(x=accuracy, y=mcc, fill = method))+
      ggplot2::geom_point(shape=21, color = "white", size = 3, alpha = 0.6)+
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim=c(0, 1))+
      ggplot2::scale_colour_hue(l = 40)+
      ggplot2::theme_minimal()

    return(metrics_plot)
  }



#' probability histogram of caret model
#'
#' @param model the caret model trained on the ms list
#' @import ggplot2
#' @return ggplot2 plot
#' @export
#' @import ggplot2
probs_hist <- function(fits){

  if (fits[[1]]$modelType == "Classification") {
    best_model <-
      fits %>%
      purrr::map("results") %>%
      dplyr::bind_rows(.id = "model") %>%
      dplyr::slice_max(Kappa) %>%
      dplyr::pull(model)
  } else {stop("This function requires classification models")}

  classes <- fits[[1]]$levels
  p <-
    ggplot2::ggplot(data = fits[[best_model]]$pred %>% tibble::as_tibble()) +
    ggplot2::geom_histogram(
      breaks = seq(0,1,length.out = 100),
      color = "gray20",
      alpha = 0.4,
      ggplot2::aes_string(x=as.character(classes[2]), fill="obs"),
      position = "identity"
    ) +
    ggplot2::theme_minimal()+
    guides(fill = guide_legend(title = "True Class"))+
    ggplot2::scale_fill_manual(values = c("#d53e4f", "#3288bd"))+
    ggplot2::labs(title = paste0("Probabilities returned by ", best_model))

  return(p)
}


#
# library(tidyverse)
# library(matlib)
#
# df <- readr::read_tsv("data/tx0c00448_si_002.edited.tsv")
# X <- df %>% select(starts_with("M")) %>% map_dfc(~log10(.x+1)) %>% as.matrix() %>% scale(center = T, scale = T)
# y <- ifelse(df$group=="control", -1, 1)
# components <- list()
# ps <- list()
# qs <- c()
# y1 = y
# x1 = X
# w1   <- t(x1)%*%y1 / norm(t(x1)%*%y1)
# W <- list()
# Q <- c()
# P <- list()
#
# for (i in 1:20){ #i=1
#
#   tk  <- (x1%*%w1)
#   ts <- t(tk)%*%tk
#   tk <- tk / as.numeric(ts)
#   pk <- t(x1)%*%tk
#   qk <- t(y1)%*%tk
#
#   if (qk == 0) {break}
#   W[[i]] <- w1
#   P[[i]] <- pk
#   Q[i] <- qk
#
#   x1 <- x1 - as.numeric(ts) * tk %*% t(pk)
#   w1 <- w1 - t(x1)%*%y1
# }
#
# W <- W %>% bind_cols %>% as.matrix
# P <- P %>% bind_cols %>% as.matrix
#
#
# B <- W%*%solve(t(P)%*%W)%*%Q
# y_pred <- X%*%B
# ggplot(tibble(pred = as.vector(y_pred), obs = factor(y)), aes(x=obs, y=pred))+geom_boxplot()
#
#
#

# df <- readr::read_tsv("data/tx0c00448_si_002.edited.tsv")
# x <- df %>% dplyr::select(dplyr::starts_with("M")) %>% as.matrix()
# x <- x^(1/4)
# x <- pqn(x)
# y <- df %>% dplyr::pull(group)
#
#
# fits <-
#   fit_models(x, y)
#
# performance_plot <-
#   plot_performance(fits)
