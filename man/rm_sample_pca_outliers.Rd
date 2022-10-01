% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_preprocess_fct.R
\name{rm_sample_pca_outliers}
\alias{rm_sample_pca_outliers}
\title{Removes outliers by PCA}
\usage{
rm_sample_pca_outliers(ms, plot = FALSE)
}
\arguments{
\item{ms}{List object containing value data and rowinfo data (meta data)}
}
\value{
ms
}
\description{
Samples are removed if: abs(x - median(x)) > (1.5 * quantile(x,0.95)-quantile(x,0.05)), where x is the PCA scores.
Scores from the first 12 components are used.
}
\details{
May be applied before row normalization but after removal of extreme features.
}
\examples{
# First convert the pneumonia object to a tibble.
pneumonia<- tibble::tibble(pneumonia)

# Generate list object
ms <- list()

# Assign feature values to ms$values
start_column <- 8 # The first column with feature values
end_column <- ncol(pneumonia) # The last column of the dataset
ms$values <- pneumonia[, start_column:end_column] \%>\% dplyr::slice(1:10)

# Assign metadata to ms$rowinfo
ms$rowinfo <- pneumonia \%>\% dplyr::select(id, group, age, gender, weight, height, BMI) \%>\% dplyr::slice(1:10) \%>\% dplyr::mutate(rowid = dplyr::row_number())
ms <- impute_zero(ms)
ms <- rm_sample_pca_outliers(ms)
}