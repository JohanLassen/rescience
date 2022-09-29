



#' Transform
#'
#' Wrapper for all the transformation methods
#'
#' @param ms
#' @param method
#'
#' @return
#'
#' @export
#'
#' @examples
transform <- function(ms, method){
  msX <- ms
  methods <- list("log10"=transform_log, "fourth root" = transform_fourth_root)
  msX$values[is.na(msX$values)] <- 0

  msX <- methods[[method]](msX)
  print(msX)
  return(msX)
}


#' Title
#'
#' @param ms
#' @param method
#'
#' @return
#' @export
#'
#' @examples
impute <- function(ms, method){
  if (method == "default") return(ms)
  methods <- list("knn" = impute_knn)
  ms$values[ms$values == 0] <- NA # we imputed by 0 and now we reverse to make a more advanced imputation
  ms <- methods[[method]](ms)
  return(ms)
}


#' Title
#'
#' @param ms
#' @param method
#'
#' @return
#' @export
#'
#' @examples
normalize <- function(ms, method){

  methods <- list("PQN" = normalize_pqn,
                  "Rownorm"=normalize_robust_row,
                  "Quantile normalization"=normalize_limma_quantile,
                  "WaveICA2"=normalize_waveICA2,
                  "Combat" = normalize_combat)
  ms <- methods[[method]](ms)
  return(ms)
}


#' Fourth root transform data
#'
#' @param ms
#'
#' @return ms
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @examples
#' fourth_root_transform(ms)
transform_fourth_root <- function(ms){
  ms$values     <- ms$values^0.25 %>% tibble::as_tibble()
  return(ms)

}


#' Log transform data
#' log(x+1)
#' @param ms
#'
#' @return ms
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @examples
#' fourth_root_transform(ms)
transform_log <- function(ms){
  ms$values     <- log(ms$values+1) %>% tibble::as_tibble()
  return(ms)

}

#' Missing value imputation: Zero replacement
#'
#' @param ms
#'
#' @return
#' @export
#'
#' @examples
impute_zero <- function(ms){
  ms$values[is.na(ms$values)] <- 0
  return(ms)
}


#' Missing value imputation: knn replacement
#'
#' @param ms
#'
#' @return
#' @export
#'
#' @examples
impute_knn <- function(ms){
  tmp1          <- ms$values %>% as.matrix() %>% t()
  tmp1[tmp1==0] <- NA
  tmp1_imputed  <- impute::impute.knn(tmp1 ,k = 10, rowmax = 0.5, colmax = 0.8, maxp = 1500, rng.seed=362436069)
  tmp1          <- tmp1_imputed$data %>% t() %>% tibble::as_tibble()
  ms$values     <- tmp1
  return(ms)
}


# impute_svd <- function(ms, k = 20){
#   feature_names <- colnames(ms$values)
#   tmp1          <- ms$values %>% as.matrix()
#   tmp1[tmp1==0] <- NA
#   tmp2          <- bcv::impute.svd(tmp1, k=k)
#   ms$values     <- tmp2$x %>% tibble::as_tibble()
#   colnames(ms$values) <- feature_names
#   return(ms)
# }

#' Missing value imputation: Random Forest replacement
#'
#' @param ms
#'
#' @return
#' @export
#'
#' @examples
impute_RF <- function(ms){
  tmp1          <- ms$values %>% as.matrix()
  tmp1[tmp1==0] <- NA
  doParallel::registerDoParallel(cores=8)
  tmp1_imputed  <-
    missForest::missForest(
      tmp1, maxiter = 10, ntree = 100, variablewise = FALSE, verbose = TRUE, parallelize = "variables"
    )

  tmp1          <- tmp1_imputed$data %>% t() %>% tibble::as_tibble()
  ms$values     <- tmp1
  return(ms)
}



#' Remove features inflated with zeros (batch-wise)
#'
#' This might help avoiding batch effect and identification of batches through signals with binary (on/off) behaviour
#'
#' @param ms
#'
#' @return ms
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @examples
#' rm_feature_batch_inflated_zeros(ms)
rm_feature_batch_inflated_zeros <- function(ms){

  raw     <- ms$values
  rowinfo <- ms$rowinfo

  tmp1 <- rowinfo %>% select(BATCH) %>%
    bind_cols(raw) %>%
    pivot_longer(cols = starts_with("M")) %>%
    group_by(BATCH, name) %>%
    summarise(zeros = mean(value==0))

  bad_features <- tmp1[tmp1$zeros>0.2,] %>% pull(name) %>% table %>%  names() %>%  unique()

  raw <- raw %>% select(-any_of(bad_features))

  ms$values <- raw
  ms$rowinfo <- ms$rowinfo

  return(ms)
}


#' Removes observations if their batches are below a given threshold
#'
#' Is usable if preceeded by batch normalization
#'
#' @param ms
#'
#' @return ms
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @examples
#' rm_sample_min_batch_occurence(ms)
rm_sample_min_batch_occurence <- function(ms, min_occurence = 5){

  tmp1 <- ms$rowinfo
  tmp2 <- ms$values

  small_batches <- table(tmp1$BATCH)[table(tmp1$BATCH) < min_occurence] %>% names
  tmp1 <- tmp1 %>% filter(!BATCH %in% small_batches)
  tmp2 <- tmp2[tmp1$rowid,]
  tmp1 <- tmp1 %>% mutate(rowid = row_number())

  ms$rowinfo <- tmp1
  ms$values <- tmp2

  return(ms)
}


#' Removes outliers by PCA
#'
#' Samples are removed if: abs(x - median(x)) > (1.5 * quantile(x,0.95)-quantile(x,0.05)), where x is the PCA scores.
#' Scores from the first 12 components are used.
#'
#' May be applied before row normalization but after removal of extreme features.
#'
#' @param ms
#'
#' @return ms
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @examples
#' rm_sample_pca_outliers(ms)
rm_sample_pca_outliers <- function(ms, plot = FALSE) {

  raw     <-  ms$values
  rowinfo <- ms$rowinfo

  tmp1 <- ms$rowinfo %>% mutate(rowid2 = row_number())
  tmp2 <- raw[tmp1$rowid,]

  r  <- prcomp(x = tmp2, retx = T, center=T, scale. = T, rank. = 12)

  bad_rows <- tibble(rowid2=apply(r$x, 2, function(x) {
    which(abs(x - median(x)) > (1.5 * quantile(x,0.95)-quantile(x,0.05)))
  }) %>%
    unlist() %>%
    as.vector()) %>%
    count(rowid2)

  tmp1 <- tmp1 %>%
    left_join(bad_rows) %>%
    mutate(n=ifelse(is.na(n), 0,n)) %>%
    mutate(label=case_when(
      n>0 ~ sample,
      n==0 ~ "")) %>%
    {.}

  pd <- r$x %>%
    tibble::as_tibble() %>%
    bind_cols(tmp1) %>%
    {.}

  pd <- pd %>%
    mutate(response = ifelse(n>0,"Outlier", "Not outlier")) %>%
    mutate(response = factor(response))

  if (plot) {
    plotlist <- list()

    for(i in 1:(ncol(r$x)/2)) {
      xvar <- names(pd)[2*i-1]
      yvar <- names(pd)[2*i]
      p1 <- ggplot(pd,aes(x=!!ensym(xvar), y=!!ensym(yvar),
                          fill=response, label=label))+
        geom_point(shape=21, color="#FFFFFFFF", size=3) +
        scale_fill_manual(values = c("#D0D0D0", "#D04040")) +
        theme(legend.position="none") +
        NULL

      plotlist[[length(plotlist)+1]] <- p1
      rm(p1)
    }

    cowplot::plot_grid(plotlist = plotlist,nrow=1)
    rm(plotlist)
  }

  bad_rows <- tmp1 %>% filter(n>0)

  if (nrow(bad_rows) > 0) {
    ms$values  <- raw[-bad_rows$rowid2,] %>% tibble::as_tibble()
    ms$rowinfo <- rowinfo[-bad_rows$rowid2,]
  }

  ms$rowinfo <- ms$rowinfo %>%
    mutate(rowid = row_number())

  return(ms)
}


#' Remove samples if internal standards are crazy
#'
#' @param ms
#' @param standards
#' e.g., M215T123
#' @param tolerance
#' Max: number of standards
#' Min: 1
#' The smaller the more restrictive. The tolerance reflects the number of crazy ISs allowed per sample.
#' @param quantiles
#'
#' @return
#' @export
#'
#' @examples
rm_IS_outliers <- function(ms, standards, tolerance = 0, quantiles = c(0.025, 0.975)){
  tmp1 <- ms$rowinfo
  tmp2 <- ms$values

  good_data <-
    tmp2 %>%
    select(all_of(standards)) %>%
    bind_cols(tmp1) %>%
    pivot_longer(cols = all_of(standards)) %>%
    mutate(batch = as.factor(BATCH))

  good_data <-
    good_data %>%
    group_by(name) %>%
    mutate(outlier = value >= quantile(value, quantiles[2]) | value <= quantile(value, quantiles[1])) %>%
    ungroup()

  a <-
    ggplot(good_data) +
    geom_point(aes(y=value, x=rowid, color = batch), show.legend = F)+
    geom_point(data = good_data %>% filter(outlier), aes(y=value, x=rowid), color = "gray40")+
    facet_wrap(~name, ncol = 1) +
    NULL
  print(a)

  bad_samples <-
    good_data %>%
    filter(outlier == TRUE) %>%
    count(rowid) %>%
    filter(n >= tolerance) %>%
    pull(rowid)

  ms$rowinfo <- ms$rowinfo %>% filter(!(rowid %in% bad_samples))
  ms$values <- ms$values[ms$rowinfo$rowid,]
  ms$rowinfo <- ms$rowinfo %>% mutate(rowid = row_number())

  return(ms)
}


#' Removes outliers by PCA
#'
#' Samples are removed if: abs(x - median(x)) > (1.5 * quantile(x,0.95)-quantile(x,0.05)), where x is the PCA scores.
#' Scores from the first 12 components are used.
#'
#' May be applied before row normalization but after removal of extreme features.
#'
#' @param ms
#'
#' @return ms
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @examples
#' rm_feature_extreme_values(ms)
rm_feature_extreme_values <- function(ms) {

  raw     <- ms$values
  rowinfo <- ms$rowinfo

  tmp1 <- tibble(rowid = rowinfo$rowid) %>%
    bind_cols(tibble::as_tibble(raw))

  tmp1 <- tmp1 %>%
    pivot_longer(names_to = "compound", values_to = "value",  cols= c(-rowid))

  tmp2 <- tmp1 %>%
    group_by(compound) %>%
    summarise(n_bad = sum(value > median(value)+2*quantile(value,0.95))) %>%
    {.}

  bad_features <- tmp2 %>%
    ungroup() %>%
    filter(n_bad > 0) %>%
    select(compound) %>%
    distinct()

  ms$values <- raw %>% select(-any_of(bad_features$compound))
  ms$rowinfo <- rowinfo

  return(ms)
}



#' Robust row normalization
#'
#' Only stable features used to normalize the rows (each sample)
#' Corrects matrix effects
#'
#' @param ms
#'
#' @return ms
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @examples
#' robust_row_normalize(ms)
normalize_robust_row <- function(ms) {

  target_info   <- ms$rowinfo
  target_values <- ms$values %>% tibble::as_tibble()

  tmp1 <- target_info
  tmp2 <- target_values[tmp1$rowid,]

  stable_features <- tmp1 %>%
    bind_cols(tmp2) %>%
    pivot_longer(starts_with("M")) %>%
    group_by(sample) %>%
    mutate(rank=rank(value)) %>%
    ungroup() %>%
    group_by(name) %>%
    summarise(median = median(rank),
              range = max(rank)-min(rank)) %>%
    ungroup() %>%
    slice_min(order_by = median, prop = 0.8) %>%
    slice_max(order_by = median, prop = 0.8) %>%
    slice_min(order_by = range, prop = 0.8)

  raw    <- target_values
  data.x <- raw
  tmp    <- rowSums(target_values %>% select(any_of(stable_features$name)))
  raw    <- max(raw)*raw / tmp

  ms$values  <- raw
  ms$rowinfo <- target_info

  return(ms)
}



#' Probabilistic quotient normalization
#'
#' @param X
#' @param n
#' @param QC
#'
#' @return
#' @export
#'
#' @examples
normalize_pqn <- function(ms, n = "median", QC = NULL) {
  X <- ms$values %>% as.matrix()
  X.norm <- matrix(nrow = nrow(X), ncol = ncol(X))
  colnames(X.norm) <- colnames(X)
  rownames(X.norm) <- rownames(X)

  if (!is.null(QC)) {
    # if QC vector exists, use this as reference spectrum
    if (length(QC) == 1) {
      # only 1 reference sample given
      mX <- as.numeric(X[QC, ])
    } else {
      if (n == "mean") {
        mX <- as.numeric(colMeans(X[QC, ]))
      }
      if (n == "median") {
        mX <- as.numeric(apply(X[QC, ], 2, median))
      }
    }
  } else {
    # otherwise use the mean or median of all samples as reference sample
    if (n == "mean") {
      mX <- as.numeric(colMeans(X))
    }
    if (n == "median") {
      mX <- as.numeric(apply(X, 2, median))
    }
  }

  # do the actual normalisation
  for (a in 1:nrow(X)) {
    X.norm[a, ] <- as.numeric(X[a, ] / median(as.numeric(X[a, ] / mX)))
  }

  ms$values <- X.norm %>% tibble::as_tibble()
  return(ms)
}


#' Normalize using waveICA2
#'
#' @param ms
#'
#' @return
#' @export
#'
#' @examples
normalize_waveICA2 <- function(ms){
  tmp1 <- ms$rowinfo
  tmp2 <- ms$values %>% as.matrix()
  tmp3 <- WaveICA2.0::WaveICA_2.0(tmp2, Injection_Order = 1:nrow(tmp2), Cutoff = 0.1, wf = "haar", K = 20, alpha = 0)
  tmp3 <- tmp3$data_wave
  ms$values <- tmp3 %>% tibble::as_tibble()
  return(ms)
}

#' Normalize using combat
#'
#' @param ms
#'
#' @return
#' @export
#'
#' @examples
normalize_combat <- function(ms){
  ms$values <-
    sva::ComBat(t(ms$values), batch = as.numeric(as.factor(ms$rowinfo$BATCH))) %>%
    t() %>%
    tibble::as_tibble()
  return(ms)
}

#' Quantile normalize using limma (quantile normalization)
#'
#' @param ms
#'
#' @return
#' @export
#'
#' @examples
normalize_limma_quantile <- function(ms){
  ms$values <- limma::normalizeBetweenArrays(as.matrix(ms$values), method = "quantile") %>% tibble::as_tibble()
  return(ms)
}


#' Cyclic loss normalize using limma
#'
#' @param ms
#'
#' @return ms
#' @export
#'
#' @examples
normalize_limma_cyclicloess <- function(ms){
  ms$values <- limma::normalizeBetweenArrays(ms$values, method = "cyclicloess")
  return(ms)
}


#' Select top n most variable features
#'
#' Effective for model screening to avoid heavy work loads. Often top 500 contains most of the signal.
#'
#' @param ms
#'
#' @return ms
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @examples
#' select_most_variable_features(ms)
select_most_variable_features <- function(ms, n=500) {
  raw <- ms$values
  rowinfo <- ms$rowinfo

  tmp1 <- rowinfo
  tmp2 <- raw[tmp1$rowid,]

  good_features <- tmp2 %>%
    bind_cols(tmp1) %>%
    pivot_longer(starts_with("M")) %>% # Convert to tidy format
    group_by(name) %>%
    summarise(mean = mean(value),
              variation = var(value)/mean(value)) %>%
    arrange(-variation) %>%
    slice(1:n) %>%
    pull(name)

  ms$rowinfo <- rowinfo
  ms$values  <- raw %>% select(any_of(good_features))

  return(ms)
}



#' Z-standardization of batches
#'
#'
#' @param ms
#'
#' @return ms
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @examples
#' normalize_batch(ms)
standardize_z_batch <- function(ms, batch_column = "BATCH") {
  raw <- ms$values
  target_info <- ms$rowinfo

  tmp1 <- tibble(batch = target_info[[batch_column]], rowid = target_info$rowid) %>%
    bind_cols(tibble::as_tibble(raw)) %>%
    pivot_longer(names_to = "compound", values_to = "value",  cols= c(-batch, -rowid))

  tmp2 <- tmp1 %>%
    group_by(batch, compound) %>%
    summarise(value.batch_mean = mean(value),
              value.batch_sd   = sd(value))


  tmp1 <- full_join(x = tmp1, y = tmp2, all.x = TRUE) %>%
    mutate(value2 = (value- value.batch_mean)/(value.batch_sd)) %>% #
    mutate(value2 = ifelse((
      value.batch_mean==0 &
        value.batch_sd==0) |
        value == 0, 0, value2
    )) %>%
    select(batch, rowid, compound,value2) %>%
    pivot_wider(names_from = compound, values_from = value2)

  raw <- tmp1 %>%
    arrange(rowid) %>%
    select(-batch, -rowid)

  ms$rowinfo <- target_info
  ms$values  <- raw
  return(ms)
}



#' PCA plot
#'
#' @param tmp1
#' @param tmp2
#' @param color_labels
#'
#' @return
#' @export
#'
#' @import dplyr cowplot
#' @importFrom ggplot2 ggplot aes_string labs theme ggdraw draw_label geom_point
#'
#' @examples
plot_pca <- function(ms, color_labels=c("AGE_YEARS", "BATCH")) {
  tmp1 <- ms$rowinfo
  tmp2 <- ms$values
  r  <- prcomp(x = tmp2, retx = TRUE, center = T, scale = T, rank. = 12)

  pd <- r$x %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(tmp1 %>% dplyr::select(dplyr::all_of(color_labels))) %>%
    {.}

  plotlist <- list()
  titles <- list()

  for(i in 1:(ncol(r$x)/2)) {
    mini_plotlist <- list()
    #titles <- list()
    for (fill in color_labels) {
      #i <- 1
      xvar <- names(pd)[2*i-1]
      yvar <- names(pd)[2*i]
      p1 <-
        ggplot2::ggplot(pd, ggplot2::aes_string(x=xvar, y=yvar, fill=fill))+
        ggplot2::geom_point(shape=21, color="#FFFFFFFF", size=2, show.legend = F) +
        ggplot2::labs(fill = fill)+
        ggplot2::theme_minimal() +
        NULL
      if (i == 1) {
        title <- cowplot::ggdraw() + cowplot::draw_label(fill)
        titles[[length(titles)+1]] <- title
      }

      mini_plotlist[[length(mini_plotlist)+1]] <- p1
      rm(p1)
    }

    aggrgte <- cowplot::plot_grid(plotlist = mini_plotlist, ncol = 1)
    plotlist[[length(plotlist)+1]] <- aggrgte
  }

  p1 <- cowplot::plot_grid(plotlist = plotlist, nrow=1)
  title <- cowplot::ggdraw() + cowplot::draw_label(paste(ncol(tmp2), "features"))
  p2 <- cowplot::plot_grid(title, p1, ncol = 1, rel_heights=c(0.1, 1))

  titles <- cowplot::plot_grid(plotlist = titles, ncol=1)
  final <- cowplot::plot_grid(titles, p2, nrow=1, rel_widths = c(2, 10))


  return(final)
}

#' Plot intensity vs injection order
#'
#' @param ms
#' @param standards
#' Feature names of standards
#'
#' @return
#' @export
#'
#' @import tidyverse
#'
#' @examples
plot_standards <- function(ms, standards) {
  values <- ms$values
  info   <- ms$rowinfo

  plot_data <-
    values %>%
    select(all_of(standards)) %>%
    bind_cols(info) %>%
    pivot_longer(cols = all_of(standards)) %>%
    mutate(batch = as.factor(as.numeric(as.factor(BATCH)) %% 2))

  p <-
    ggplot(plot_data) +
    geom_point(aes(y=value, x=rowid, fill = batch, color = batch), shape = 21, show.legend = F)+
    facet_wrap(~name, ncol = 1) +
    scale_fill_manual(values = c("#ffffcc", "#41b6c4"))+
    scale_color_manual(values = c("#7fcdbb","#225ea8")) +
    theme_classic()
  NULL

  return(p)
}


#' UMAP of data
#'
#' @param tmp1
#' @param tmp2
#' @param color_label
#'
#' @return
#' @export
#' @import tidyverse
#' @import umap
#' @examples
make_umap <- function(tmp1, tmp2, color_label) {
  r <- umap::umap(tmp2)
  o <- tibble("umap1"=r$layout[,1],
              "umap2"=r$layout[,2])
  pd <- o %>%
    bind_cols(tmp1) %>%
    {.}

  p <- ggplot(pd,aes(x=umap1, y=umap2,
                     fill=.data[[color_label]]))+
    geom_point(shape=21, color="#FFFFFFFF", size=3, show.legend = F) +
    labs(fill = color_label)+
    theme() +
    NULL
  return(p)
}


#' probability histogram of caret model
#'
#' @param model
#'
#' @return
#' @export
#' @import tidyverse
#' @examples
probs_hist <- function(model){
  classes <- unique(model_cv_r$obs)
  ggplot(data = model$pred %>% tibble::as_tibble()) +
    geom_histogram(
      breaks = seq(0,1,length.out = 100),
      color = "gray20",
      alpha = 0.4,
      aes_string(x=as.character(classes[2]), fill="obs"),
      position = "identity"
    ) +
    theme_minimal()+
    scale_fill_manual(values = c("#d53e4f", "#3288bd"))+
    labs(title = best_model)
}


#' ROC of caret model
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
plot_roc <- function(model){
  # ROC CURVE - ALL MODELS
  classes <- unique(model_cv_r$obs)
  g <- ggplot(model$pred,
              aes_string(m=classes[1],
                         d=factor("obs", levels = c(as.character(classes[1]), as.character(classes[2]))),
                         color = "model")
  ) +
    geom_roc(n.cuts=0) +
    coord_equal() +
    style_roc()

  g +
    annotate("text", label=paste("ROC", " =", round((calc_auc(g))$AUC, 4)))+
    scale_color_brewer(palette = "Spectral")
}

#' Importance from caret model
#'
#' @param model
#'
#' @return
#' @export
#' @import tidyverse
#' @import caret
#' @examples
plot_importance <- function(model){

  # FEATURE IMPORTANCE - BEST MODEL
  importance <- varImp(model) %>%
    {
      tibble(Feature = rownames(.$importance),
             Importance = .$importance %>% unlist %>% round(2)) %>%
        arrange(desc(Importance))
    }


  importance[1:10,] %>%
    ggplot(aes(y=factor(Feature, levels = rev(importance$Feature)), x=Importance)) +
    geom_col()+
    labs(y = "Features")+
    theme_minimal()

  important <- importance %>% filter(Importance>5) %>% arrange(-Importance) %>% slice(1:25)



  outcome <- model %>% select(obs, rowIndex) %>% distinct() %>% arrange(rowIndex)

  outcome %>%
    tibble::as_tibble() %>%
    bind_cols(model$trainingData) %>%
    select(obs, all_of(important$Feature)) %>%
    filter(!is.na(obs)) %>%
    pivot_longer(names_to = "Feature", cols=-obs) %>%
    ggplot() +
    geom_boxplot(aes(x=obs, y=value), fill = "#6DA398", show.legend = F)+
    facet_wrap(~Feature, scales = "free_y")+
    theme_bw()
}


#' Scatter plot of probabilities
#'
#' Used when classifying over a continuous variable
#'
#' @param model
#' @param target_info
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
prob_vs_continuous <- function(model, target_info, var_name){
  pdata <-
    model$pred %>%
    arrange(rowIndex) %>%
    mutate(rowid = rowIndex) %>%
    inner_join(target_info %>% drop_na() %>% mutate(rowid = row_number()), by = "rowid")

  classes <- unique(model_cv_r$obs)

  ggplot(pdata, aes_string(y=classes[1], x = var_name, fill = "obs")) +
    geom_point(shape = 21, color = "white") +
    scale_fill_manual(values = c("#d53e4f", "#3288bd"))+
    geom_segment(aes(x = min(age), xend = max(age), y=0.5, yend = 0.5), color = "black", linetype = "dashed")
}

