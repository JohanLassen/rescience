
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


fit_models <- function(x, y, methods = c("glmnet", "pls")){
  cat("\nNotice:\nDepending on the data size this process may take 1-60 minutes\n\n")
  trctrl <- caret::trainControl(method = "repeatedcv", number = 3, repeats = 2, verboseIter = T, savePredictions = T, classProbs = T)
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

get_performance <-
  function(fits){
    predictions <-
      fits %>%
      purrr::map(5) %>%
      bind_rows(.id = "method") %>%
      mutate(
        rep = gsub(".*[.]", "", Resample),
        fold = gsub("[.].*", "", Resample)
      )

    performance <- predictions %>%
      group_by(method, rep) %>%
      summarise(accuracy = mean(pred == obs), mcc = mcc(pred, obs))

    return(performance)
  }


plot_performance <-
  function(fits){

    performance <-
      get_performance(fits)

    metrics_plot <-
      performance %>%
      ggplot(aes(x=accuracy, y=mcc, fill = method))+
      geom_point(shape=21, color = "white", size = 2)+
      scale_colour_hue(l = 45)+
      theme_minimal()

    rocdata <-
      MLeval::evalm(fits, silent = T, showplots = F, gnames = names(fits))$roc$data

    roc_curve <-
      rocdata %>%
      ggplot(aes(x=FPR, y=SENS, color = Group))+
      geom_abline(color="gray")+
      geom_line(show.legend = F)+
      scale_colour_hue(l = 45) +
      theme_minimal()+
      labs(x="false postive rate", y="true positive rate")

    combined_diagnostics <-
      cowplot::plot_grid(roc_curve, metrics_plot)

    return(combined_diagnostics)
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
