% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alpha.R
\name{alpha_index}
\alias{alpha_index}
\title{alpha}
\usage{
alpha_index(otu, tree)
}
\arguments{
\item{otu}{otu table}

\item{tree}{tree}
}
\value{
alpha dist
}
\description{
compute some indexs.
}
\examples{
data(its)
indexs <- alpha_index(its)
indexs
}
