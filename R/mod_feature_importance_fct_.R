

#
#
# ms <- load_ms(pneumonia, outcome = "group", feature_start = 8, analysis_type = "classification")
# ms <-
#   ms %>%
#   transform_log %>%
#   impute_knn %>%
#   normalize_robust_row
#
# fits <- fit_models(ms, methods = c("glmnet", "pls"))
#
# selected_model <- "glmnet"

get_important <- function(fits){
  topX_important <-
    fits %>%
    purrr::map(caret::varImp) %>%
    purrr::map(1) %>%
    purrr::map(as_tibble, rownames = "feature") %>%
    purrr::map(slice_max, order_by = Overall, n=20) %>%
    bind_rows(.id = "model")

  return(topX_important)
}

feature_importance_global <-function(fits, selected_model){
  topX_important <- get_important(fits)
  topX_important %>%
    filter(model == selected_model) %>%
    mutate(feature = factor(feature, levels = feature[order(Overall)])) %>%
    ggplot(aes(x=Overall, y=feature))+
    geom_col()+
    theme_minimal()+
    labs(x="Normalized Importance", title = paste0("Feature selection from ", selected_model))
}




#' Boxplot of ML selected features
#'
#' @param ms ms list object
#' @param feature vector with feature names from ms$values
#'
#' @return a ggplot
#' @export
#'
#' @examples
feature_boxplot <-
  function(ms, feature=NULL){
    if (is.null(feature)){
      feature <- get_important(fits) %>% filter(model=="glmnet") %>% slice_max(Overall, n=4) %>% pull(feature)
    }
    ms$values %>%
      as_tibble() %>%
      mutate(Class = ms$rowinfo[[ms$info$outcome]]) %>%
      select(Class, feature) %>%
      tidyr::pivot_longer(cols = -Class) %>%
      mutate(name = factor(name, levels = feature)) %>%
      ggplot(aes(x=value, y=name, fill=Class))+
      geom_boxplot(position="dodge")+
      theme_minimal()+
      scale_fill_brewer(palette = "Set1")+
      labs(y="", x="Intensity")
  }


#' Internal function to get feature feature correlations for post processing of sparse ML feature selection
#'
#' @param ms ms list object
#' @param features feature vector containg column names from ms$values
#'
#' @return
maxabs_feature_correlations <-
  function(ms, features){
    feature_feature_correlations <-
      ms$values %>%
      stats::cor() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(feature1 = colnames(.)) %>%
      dplyr::filter(feature1 %in% features) %>% select(-feature1) %>%
      purrr::map_dfc(~{.x[.x==1] <- 0; return(.x)}) %>%
      purrr::map(abs) %>% purrr::map_dbl(max) %>% sort(decreasing=TRUE)
    return(feature_feature_correlations)
  }


heatmap_pathway <-
  function(ms,features){

    feature_cor <- maxabs_feature_correlations(ms, features)[1:20] %>% names()

    include_features <- unique(c(features, feature_cor))
    values <- cor(ms$values)
    values <- values[colnames(values) %in% include_features, colnames(values) %in% features]

    return(pheatmap::pheatmap(values, silent = T))
  }




correlation_map <-
  function(ms, features){
    x <- ms$values
    ffcor_long <-
      maxabs_feature_correlations(ms, features) %>%
      filter(best_cor > 0.8) %>%
      mutate(value = ifelse(value == 0, 1, value))

    ffcor_wide <- ffcor_long %>% select(-best_cor) %>% tidyr::pivot_wider(names_from="feature1", values_from="value") %>% select(-name) # The ml selected features

    clusters <- factoextra::fviz_nbclust(t(ffcor_wide), FUNcluster = kmeans, kmax = nrow(t(ffcor_wide)))

    clusters <- which.max(clusters$data$y)
    km       <- kmeans(t(as.matrix(ffcor_wide)), centers = clusters) # could say "clusters"
    order    <- km$cluster

    d1 <- data.frame(
      from = "origin",
      to=paste("group", c(1:clusters), sep="_")
    )
    d2 <-
      ffcor_long %>%
      select(-best_cor, -value, to = feature, from = name) %>%
      mutate(from = paste("group", order[match(to, names(order))], sep="_")) %>%
      mutate(id = as.numeric(gsub(".*_", "", from))) %>%
      arrange(id) %>%
      select(-id)
    edges <- rbind(d1, d2)

    # create a dataframe with connection between leaves (individuals)
    connect <-
      ffcor_long %>%
      select(-best_cor, -value, to = feature, from = name)
    connect$value <- ffcor_long$value

    # create a vertices data.frame. One line per object of our hierarchy
    vertices  <-  data.frame(
      name = unique(c(as.character(edges$from), as.character(edges$to)))
    )

    # Calculating fold change
    fc <-
      x[, vertices$name[grepl("M", vertices$name)]] %>%
      as_tibble() %>%
      split(y) %>%
      purrr::map(~.x %>% purrr::map_dfc(~mean(.x))) %>%
      bind_rows(.id="group") %>%
      tidyr::pivot_longer(cols=-group) %>%
      tidyr::pivot_wider(values_from=value, names_from = group) %>%
      mutate(fold_change = -log2(control/pneumoni)) %>%
      pull(fold_change)

    vertices$value <- c(rep(NA, (nrow(vertices)-length(fc))), fc)

    # Let's add a column with the group of each name. It will be useful later to color points
    vertices$group  <-  edges$from[ match( vertices$name, edges$to ) ]

    vertices$id <- NA
    myleaves <- which(is.na( match(vertices$name, edges$from) ))
    nleaves <- length(myleaves)
    vertices$id[ myleaves ] <- seq(1:nleaves)
    vertices$angle <- - 360 * vertices$id / (nleaves)

    # calculate the alignment of labels: right or left
    # If I am on the left part of the plot, my labels have currently an angle < -90
    vertices$hjust <- rep(0, length(vertices$angle))
    #vertices$hjust <- ifelse( vertices$angle < 90, 1, 0)

    # flip angle BY to make them readable
    # Correct vector
    v <- vertices$angle < -80 & vertices$angle > -270
    vertices$angle <- ifelse(v, vertices$angle+180, vertices$angle)
    vertices$hjust <- ifelse(v, 1, 0)

    # Create a graph object
    mg <-
      igraph::graph_from_data_frame( edges, vertices=vertices ) %>%
      tidygraph::as_tbl_graph(mygraph) %>%
      tidygraph::activate(nodes) %>%
      mutate(
        `feature-outcome fold change` = c(rep(NA, (nrow(vertices)-length(fc))), fc),
      )

    # The connection object must refer to the ids of the leaves:
    # from  <-  match( connect$from, vertices$name)
    # to    <-  match( connect$to, vertices$name)
    # value1 <-  ffcor_long$value
    # length(from)
    # length(value)

    graph_info <- mg %>% tidygraph::activate(edges) %>% data.frame()
    graph_info <- graph_info %>% mutate("feature-feature correlations" = c(rep(NA, (nrow(graph_info)-length(ffcor_long$value))), ffcor_long$value))
    library(ggraph)
    graph_plot <-
      ggraph::ggraph(mg, layout = 'dendrogram', circular = TRUE) +
      ggraph::geom_conn_bundle(
        data = do.call(ggraph::get_con,graph_info),
        alpha=0.2,
        width=0.9,
        aes(colour=`feature-feature correlations`)) +
      ggraph::scale_edge_colour_distiller(palette = "YlGnBu", direction = 1) +

      ggraph::geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, hjust=hjust, angle=angle), size=2, alpha=1) +

      ggraph::geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=`feature-outcome fold change`), size=6, show.legend = T) +
      scale_color_distiller(palette="Spectral")+
      scale_size_continuous( range = c(0.1,10) ) +
      theme_void() +
      theme(
        plot.margin=unit(c(1,1,1,1),"cm"),
      ) +
      expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

    return(graph_plot)
  }

#
#
# features <- c("M364T419",
#               "M363T419",
#               "M365T392",
#               "M394T590",
#               "M192T138",
#               "M366T392")
#
# plot.new()
# a <- heatmap_pathway(ms, c("M364T419",
# "M363T419",
# "M365T392",
# "M394T590",
# "M192T138",
# "M366T392"))
#
# a$tree_row
#
#
# correlation_map(ms, features)
