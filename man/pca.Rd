% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca.R
\name{pca_plot}
\alias{pca_plot}
\alias{pcoa_plot}
\alias{nmds_plot}
\title{PCA PCoa NMDS}
\usage{
pca_plot(
  df,
  group,
  scale = FALSE,
  axis = c(1, 2),
  label = TRUE,
  legend = FALSE,
  point_size = 2,
  font = "sans",
  font_size = 10
)

pcoa_plot(
  dist,
  group,
  axis = c(1, 2),
  label = TRUE,
  legend = FALSE,
  point_size = 2,
  font = "sans",
  font_size = 10,
  ...
)

nmds_plot(
  dist,
  group,
  label = TRUE,
  legend = FALSE,
  point_size = 2,
  font = "sans",
  font_size = 10,
  ...
)
}
\arguments{
\item{df}{df}

\item{group}{group}

\item{scale}{TRUE of FALSE. Scale species to unit variance
(like correlations).}

\item{axis}{select the axis to plot}

\item{label}{TRUE of FALSE. Whether to draw a label}

\item{legend}{TRUE of FALSE. Whether to draw a legend}

\item{point_size}{point size}

\item{font}{sans = Arial, serif = Times New Roman}

\item{font_size}{font size}

\item{dist}{dist.}

\item{...}{Other params from ?ape::pcoa and ?vegan::metaMDS}
}
\description{
Plot the PCA PCoa NMDS.
}
\examples{
data(its)
otu_t <- t(its)
pca_plot(otu_t)
bray_dist <- vegan::vegdist(otu_t)
pcoa_plot(bray_dist)
nmds_plot(bray_dist)
}
