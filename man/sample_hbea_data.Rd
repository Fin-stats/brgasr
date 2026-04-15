\name{sample_hbea_data}
\alias{sample_hbea_data}
\title{Load the Bundled HBEA Sample Dataset}
\description{
Loads a bundled synthetic 10-year return series designed to illustrate package
usage.
}
\usage{
sample_hbea_data()
}
\value{
A data frame with columns \code{date}, \code{ret}, and \code{post_break}.
}
\examples{
sample_df <- sample_hbea_data()
head(sample_df)
}
