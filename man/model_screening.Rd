% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_machine_learning_fct.R
\name{model_screening}
\alias{model_screening}
\title{Model screening}
\usage{
model_screening(ms, methods, outcome, analysis_type, tech_rep = NULL)
}
\arguments{
\item{ms}{ms list object containing value and meta data}

\item{methods}{machine learning methods to fit}

\item{outcome}{outcome variable to predict}

\item{tech_rep}{makes sure technical replicates end up together in hold out data to prevent data leakage}
}
\value{
list of caret models
}
\description{
Model screening
}
